/' -- particle framework - 2025 Jan 23 - by dafhi

    License:  Use, modify, profit.

        Inspired by
        
    1. desire for simplification (my particle interest dates to 2004-ish)
    2. ongoing search for my holy grail of fixed-rate emitters:  fps-congruent acceleration
    
        features

    3d vector math
    fixed-rate physics
    every particle is an emitter
    velocity transfer
    per-particle FPS
    set emission (& duration) at any time
    set acceleration (& duration) at any time
    auto recycle
    
        Things that will likely change
    
    _spawn() subroutine which i threw together
    set_accel algorithm (far-off future unless my IQ suddenly jumps)
  
  
        - update -
        
    moved a bunch of stuff to precede the namespace
    renamed a bunch of stuff
    moved emitter.spawn_vel & .spawn_dv to shared variables
    set_accel precalc
    add_particle e.vel precalc
    add_particle rand_pos0 parameter
    
'/

'#include "v3_base.bas"

type p3 as v3


type v3
  declare sub       rand_on_sphere( as single = 1 )

  as single         x,y,z
End Type
    const tau = 8 * Atn(1)

sub v3.rand_on_sphere( f as single ):  y = 2*(rnd-.5) : var r = f*sqr(1-y*y)
  z=rnd*tau : x=r*cos(z) : z=r*sin(z) : y *= f
End Sub

operator -( byref L as p3, byref R as p3 ) as v3 : return type( L.x-R.x, L.y-r.y, L.z-R.z ) : end operator
operator +( byref L as v3, byref R as v3 ) as v3 : return type( L.x+R.x, L.y+r.y, L.z+R.z ) : end operator
operator /( byref L as v3, r as single ) as v3 : return type( L.x/r, L.y/r, L.z/r ) : end operator
operator *( byref L as v3, r as single ) as v3 : return type( L.x*r, L.y*r, L.z*r ) : end operator
operator *( L as Single, byref R as v3 ) as v3 : return type( L*r.x, L*r.y, L*r.z ) : end operator

' ---------------

    function max( a as double, b as double ) as double
        return iif( (a)>(b), (a), (b) )
    end function

    function min( a as double, b as double ) as double
        return iif( (a)<(b), (a), (b) )
    end function

      '' velocity precalc
    function geometric_sum( k as single = .5, n as single = 2) as double
        return (k-k^(n+1)) / (1-k)
    end function


type emitter
    
    declare sub k_transfer( as v3, as single, as single, as single = 1 )
    
    as p3       pos           '' basic particle properties
    as v3       vel
    as single   life
    
    as v3       accel         '' extra spicy physics
    
    as single   _k            '' fixed-rate physics
    as single   _one_over_fps
    
    as single   _lt_phys      '' if life <= this, calc physics frame
    
    as single   _pps          '' particles per sec
    as single   _t_accel      '' this <= 0, stop acceleration
    as single   _t_emit       '' this <= 0, stop emission

end type

    dim shared as emitter  a_emi(299999)
    dim shared as long     i_active = -1

    dim shared as single   spawn_vel = 50 '' 2 demo variables
    dim shared as single   spawn_dv = 25


  ' A main sub
  '
sub new_particle( p0 as v3, v as v3, particle_density as single, physics_fps as single, life as single = 1, rand_pos0 as single = 0 )
    
    if i_active >= ubound(a_emi) then exit sub
    
    i_active += 1
    dim byref as emitter e = a_emi(i_active)
    
    e._one_over_fps = 1 / physics_fps
    e.life = life - rand_pos0 * e._one_over_fps
    e._lt_phys = e.life - e._one_over_fps       ' physics frame trigger
    e._k = particle_density ^ e._one_over_fps
    
    e.vel = v / geometric_sum( e._k, physics_fps ) ' accurate - 2025 Jan 23
    'e.vel = v * e._one_over_fps                    ' approximate
    
    e.pos = p0 + e.vel * rand_pos0
end sub
    
    sub set_accel( e as emitter, a as v3, t as single )
        dim as single fps = 1 / e._one_over_fps
        dim as single k = e._k ^ fps
        e.accel = a * e._one_over_fps ^ 2 '' 2025 Jan 23
        e._t_accel = t
    end sub
    
    sub set_emission( e as emitter, pps as single, t as single )
        e._pps = pps
        e._t_emit = t
    end sub
    
    function f_velnorm( byref e as emitter ) as  v3 '' 2025 Jan 23
        dim as single fps = 1 / e._one_over_fps
        dim as single k = e._k ^ fps
        return e.vel * fps * k
    end function

sub emitter.k_transfer( v0 as v3, density as single, phys_fps as single, life as single )
    if i_active >= ubound(a_emi) then exit sub
    dim as single rand_pos0 = rnd
    new_particle pos, f_velnorm(this) + v0, density, phys_fps, life, rand_pos0
end sub


        namespace ns_particles '' 2025 Jan 23

    sub phys_frame( e as emitter ) '' fast approxximate terminal velocity.  sometimes i reorder
        e.vel += e.accel
        e.vel *= e._k
        e.pos += e.vel
    end sub
    
    sub accel_timer( e as emitter, dt as single )
        e._t_accel -= dt
        if e._t_accel <= 0 then
            e.accel = type(0,0,0)
            e._t_accel = 0
        EndIf
    End Sub
    
    sub emission_timer( e as emitter, dt as single )
        e._t_emit -= dt
        if e._t_emit <= 0 then
            e._pps = 0
            e._t_emit = 0
        EndIf
    End Sub
    
    sub _phys( e as emitter, dt as single )
        e.life -= dt
        while dt > 0
            phys_frame e
            accel_timer e, min( dt, e._one_over_fps )
            emission_timer e, min( dt, e._one_over_fps )
            e._lt_phys -= e._one_over_fps
            dt -= e._one_over_fps
        wend
    End Sub
      
    sub _spawn( e as emitter, dt as single, p0 as p3, dpos as v3 )
        static as v3 v
        
        dt *= e._pps
            while dt > 0
        dim as single f = rnd
        if f < (dt) then
            
            v.rand_on_sphere( spawn_vel + rnd * spawn_dv ) '' float scalars
            
            dim as single density = .3 + .05 * rnd
            dim as single phys_fps = 25
            dim as single life = 1.5 + rnd * .5
            e.k_transfer v, density, phys_fps, life
        EndIf
        dt -= (.5 + rnd)
        Wend
    end sub
    
        function f_dpos( e as emitter, dt as single ) as v3
            return e.vel * dt * e._k
        End Function
    
    sub _fixedrate_phys( e as emitter, dt as single )
        dim as single t_zero = e.life - dt
        
        if t_zero > e._lt_phys then '' no position update, but maybe spawn
        
            e.life = t_zero
            _spawn e, dt, e.pos, f_dpos(e, dt)
            accel_timer e, dt
            emission_timer e, dt
        else
            dim as p3     spawn_pos0 = e.pos
            _phys e, dt
            _spawn e, dt, spawn_pos0, e.pos - spawn_pos0
        endif
    end sub
    
    sub _recycle
        dim as long i
            while i <= i_active
        dim byref as emitter e = a_emi(i)
        if e._one_over_fps andalso e.life <= 0 then
            e = a_emi(i_active)
            i_active -= 1
        else
            i += 1
        endif
        wend
    end sub

sub update( dt as single )
    for i as long = 0 to i_active
        _fixedrate_phys a_emi(i), dt
    next
    _recycle
end sub

sub draw
    for i as long = 0 to i_active
      pset ( a_emi(i).pos.x, a_emi(i).pos.y )
    next
end sub

end namespace

  function round(in as double, places as ubyte = 2) as string '' mostly for debug / print
    dim as integer mul = 10 ^ places
    return str(csng( int(in * mul + .5) / mul) )
  End Function



dim as long  w = 800
dim as long  h = 600

screenres w,h, 32


dim as long   y_ = h - 50

dim as long   u = 4
dim as string str_fps_array(u)
dim as long   rocket_x_array(u)


dim as single size = sqr(w*w + h*h) / 10
dim as single life = 8
dim as single dens = 0.4

dim as v3     v = type(0,1,0) * -h / 15


    '' rockets
for i as long = 0 to u
    
    dim as single fps = (i+.1) * 10 '' fps profiles
    
    str_fps_array(i) = round(fps)  '' printout
    rocket_x_array(i) = 50 + 150*i
    
    '' A main sub
    new_particle type( rocket_x_array(i), y_ - 2), v*0, dens, fps, life
    
    dim byref as emitter e = a_emi(i)
    
    '' secondary sub
    set_accel e, v*8, 1.4               '' duration
        
        var particles_per_second = 999
    '' secondary sub
    set_emission e, particles_per_second, 5 '' duration
    
    spawn_vel = (.2) * size
    spawn_dv = (.1) * size
next

    dim as double t = timer, t0 = t, t1 = t + 10


while t < t1

        screenlock
    cls
    line (1,y_)-(w-2,y_)
    draw string (5, y_ + 10), " FPS"

    for i as long = 0 to u
        dim byref as emitter e = a_emi(i)
        draw string ( rocket_x_array(i), y_ + 10 ), str_fps_array(i)
    next


    ns_particles.draw         '' a main sub
    screenunlock

    dim as double t = timer
    ns_particles.update t - t0 '' a main sub
    t0 = t


    var kstr = inkey
    if kstr <> "" or i_active < 0 then exit while
    sleep 1
wend


locate 3,2
print "Done!"

sleep 2000

/' -- particle framework - 2025 Jan 22 - by dafhi

    inspired by my previous attempts,
    the desire for simplification,
    and my ongoing search for 'the' holy grail of fixed-rate emitters .. fps-congruent acceleration.
  
        key features of my (January 2025) system

    every particle is an emitter
    super tight physics (see phys_frame)
    velocity transfer
    congruent-ish acceleration abbove 20 (fixed rate) fps .. (extreme accel or density may call for higher)
    proof-of-concept per-particle FPS
    set emission (& duration) at any time
    set acceleration (& duration) at any time
    
        Things that will likely change
    
    move emitter type outside the namespace so peeps will feel free to modify.
    _spawn() subroutine which i threw together
    remove emitter.spawn_vel and .spawn_dv
    set_accel precalcs (far-off future unless my IQ suddenly jumps)
  
'/

  
function max( a as double, b as double ) as double
    return iif( (a)>(b), (a), (b) )
end function

function min( a as double, b as double ) as double
    return iif( (a)<(b), (a), (b) )
end function

'#include "v3_base.bas"
    const tau = 8 * Atn(1)

type p3 as v3


type v3
  declare sub       rand_on_sphere( as single = 1 )

  as single         x,y,z
End Type

sub v3.rand_on_sphere( f as single ):  y = 2*(rnd-.5) : var r = f*sqr(1-y*y)
  z=rnd*tau : x=r*cos(z) : z=r*sin(z) : y *= f
End Sub


operator -( byref L as p3, byref R as p3 ) as v3 : return type( L.x-R.x, L.y-r.y, L.z-R.z ) : end operator
operator +( byref L as v3, byref R as v3 ) as v3 : return type( L.x+R.x, L.y+r.y, L.z+R.z ) : end operator
operator /( byref L as v3, r as single ) as v3 : return type( L.x/r, L.y/r, L.z/r ) : end operator
operator *( byref L as v3, r as single ) as v3 : return type( L.x*r, L.y*r, L.z*r ) : end operator
operator *( L as Single, byref R as v3 ) as v3 : return type( L*r.x, L*r.y, L*r.z ) : end operator


function sgs( k as single, n as single) as single

  /'   -- sum of geometric sum.  i use it to represent sum of velocities
     
     inputs    particle density, time
     output:   position
  '/
    static as single r:  r = k/(1*(1-k))
  return r * ( n - r*(1-k^n) )
 
  '   -- theory -
  ' geometric sum    [sum of geometric sum]
  ' k*(1-k^1)/(1-k)   + .. k*(1-k^n)/(1-k)

end function


    namespace dafhis_particle_system '' 2025 Jan 21


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

    as single   spawn_vel = 50
    as single   spawn_dv = 25
end type

    dim as emitter  a_emi(299999)
    dim as long     i_active = -1


    sub add( p0 as v3, v as v3, particle_density as single, physics_fps as single, life as single = 1 )', burn_in as single = 0 )
        
        if i_active >= ubound(a_emi) then exit sub
        
        i_active += 1
        dim byref as emitter e = a_emi(i_active)
        dim as single rand_pos0 = rnd
        
        e._one_over_fps = 1 / physics_fps
        e.life = life - rand_pos0 * e._one_over_fps
        e._lt_phys = e.life - e._one_over_fps       ' physics frame trigger
        e._k = particle_density ^ e._one_over_fps
        e.vel = v * e._one_over_fps               ' approximate
        e.pos = p0 + e.vel * rand_pos0
    end sub
    
    sub set_accel( e as emitter, a as v3, t as single )
        dim as single fps = 1 / e._one_over_fps
        dim as single k = e._k ^ fps
        e.accel = a * sgs( k, e._one_over_fps ) * e._one_over_fps
        e._t_accel = t
    end sub
    
    sub set_emission( e as emitter, pps as single, t as single )
        e._pps = pps
        e._t_emit = t
    end sub
    
    function f_velnorm( byref e as emitter ) as  v3
        return e.vel * e._k / e._one_over_fps
    end function

sub emitter.k_transfer( v0 as v3, density as single, phys_fps as single, life as single )
    if i_active >= ubound(a_emi) then exit sub
    add pos, f_velnorm(this) + v0, density, phys_fps, life
end sub


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
                dim as single density = .1 + .05 * rnd
                dim as single phys_fps = 25
                dim as single life = 1.5 + rnd * .5
                dim as single vel = e.spawn_vel + rnd * e.spawn_dv
                v.rand_on_sphere( vel )
                e.k_transfer v, density, phys_fps, life
            EndIf
            dt -= (.5 + rnd)
            Wend
        end sub
    
        function f_dpos( e as emitter, dt as single ) as v3
            return e.vel * dt * e._k
        End Function
    
    sub fixedrate_phys( e as emitter, dt as single )
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
        fixedrate_phys a_emi(i), dt
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

dim as single size = sqr(w*w + h*h) / 10

dim as single life = 8
dim as single dens = 0.35
dim as v3     v = type(0,2,0) * -h / 8

dim as long y_ = h - 50

dim as long u = 4
dim as string str_fps_array(u)
dim as long   rocket_x_array(u)

    #define ps dafhis_particle_system
for i as long = 0 to u
    
    '' fps profiles
    dim as single fps = (i+.1) * 10
    
    '' printout
    str_fps_array(i) = round(fps)
    rocket_x_array(i) = 50 + 150*i
    
    ps.add type( rocket_x_array(i), y_ - 2), v, dens, fps, life
    dim byref as ps.emitter e = ps.a_emi(i)
    
    ps.set_accel e, v*12, .75
        
        var particles_per_second = 999
    ps.set_emission e, particles_per_second, 2.25
    
next

dim as double t = timer, t0 = t, t1 = t + 10
    
while t < t1

screenlock
    cls
    line (1,y_)-(w-2,y_)
    draw string (5, y_ + 10), " FPS"

    for i as long = 0 to u
        dim byref as ps.emitter e = ps.a_emi(i)
        draw string ( rocket_x_array(i), y_ + 10 ), str_fps_array(i)
    next

    ps.draw
screenunlock

    dim as double t = timer
    ps.update t - t0
    t0 = t

    var kstr = inkey
    if kstr <> "" or ps.i_active < 0 then exit while
    sleep 1
wend


locate 3,2
print "Done!"

sleep 2000

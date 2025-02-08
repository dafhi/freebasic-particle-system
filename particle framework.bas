/' -- freebasic particle FX framework - 2025 Feb 8 - by dafhi

    License:  use, modify, profit
    
      - features

    set acceleration or emission at any time
    vector math

      - update
      
    emission and accel timing

    - will likely change

    _emit_multiple() subroutine, created in a demo-ish way
    set_accel algorithm (far-off future unless my IQ suddenly jumps)
    f_velnorm() .. produces inaccurate frame to unit time (1 sec) conversion
    
'/


'#include "freebasic_particles_FX.bas"
    const tau = 8 * Atn(1)

type p3 as v3

type v3
    declare operator  cast as string                  '' optional (debugger)
    declare sub       rand_on_sphere( as single = 1 ) '' optional (explosion)

    as single         x,y,z
End Type

operator v3.cast as string
    return "x y z " + str(x) + " " + str(y) + " " + str(z)
end operator

sub v3.rand_on_sphere( f as single ):  y = 2*(rnd-.5) : var r = f*sqr(1-y*y)
  z=rnd*tau : x=r*cos(z) : z=r*sin(z) : y *= f
End Sub

operator +( byref L as v3, byref R as v3 ) as v3 : return type( L.x+R.x, L.y+r.y, L.z+R.z ) : end operator
operator /( byref L as v3, r as single ) as v3 : return type( L.x/r, L.y/r, L.z/r ) : end operator
operator *( byref L as v3, r as single ) as v3 : return type( L.x*r, L.y*r, L.z*r ) : end operator
operator *( L as Single, byref R as v3 ) as v3 : return type( L*r.x, L*r.y, L*r.z ) : end operator

    function max( a as double, b as double ) as double
        return iif( (a)>(b), (a), (b) )
    end function

    function min( a as double, b as double ) as double
        return iif( (a)<(b), (a), (b) )
    end function

    function geometric_sum( k as single, n as single) as double
        return (k-k^(n+1)) / (1-k)
    end function


type emitter
    
    as p3       pos           '' basic particle properties
    as v3       vel
    as single   life
    
    as ulong    argb          '' new (2025 )
    
    as v3       accel         '' spice
    
    as single   _k            '' fixed-rate density precalc
    as single   _one_over_fps
    
    as single   _t_phys       '' if life <= this, do physics
    
    as single   _pps          '' particles per sec
    as single   _t_accel      '' this <= 0, stop acceleration
    as single   _t_emit       '' this <= 0, stop emission

end type

    dim shared as emitter a_emi(299999)
    dim shared as long    i_active = -1

    dim shared as single  spawn_vel = 50 '' demo variables
    dim shared as single  spawn_dv = 25
    
  
    function f_vlerp( v0 as v3, v1 as v3, f as single ) as v3
        return v0 * (1-f) + v1 * f
    end function
        dim shared as single  _age

    sub _vel_transfer( e as emitter, v_explo as v3, fps as single, vnorm_parent as v3, anorm_parent as v3 )
        dim as single f = 1 / geometric_sum( e._k, fps)
        dim as v3 v0 = ((v_explo + vnorm_parent) * f)
        e.vel = f_vlerp( v0, (v0+anorm_parent * f) * e._k, _age )
    end sub


sub new_particle( p0 as v3, v_explo as v3, particle_density as single, physics_fps as single, life as single = 1, age as single = 0, vNorm_parent as v3 = type(0,0,0), aNorm_parent as v3 = type(0,0,0) )
    if i_active >= ubound(a_emi) then exit sub
    i_active += 1
    dim byref as emitter e = a_emi(i_active)
    e.pos = p0
    e._one_over_fps = 1 / physics_fps
    e._k = particle_density ^ e._one_over_fps
    e.life = life - age
    e._t_phys = e.life - e._one_over_fps
    _vel_transfer e, v_explo, physics_fps, vNorm_parent, aNorm_parent
end sub
    
    sub set_accel( e as emitter, a as v3, t as single )
        dim as single k = e._k ^ (1/e._one_over_fps)
        e.accel = a * (e._one_over_fps ^ 2 / e._k) '' not quite the fps-congruent holy grail
        e._t_accel = t
    end sub
    
        function f_velnorm( v as v3, e_k as single, fps as single ) as v3
            return (fps * e_k ^ fps) * v
            'return (fps / e_k) * v
        end function
          
        sub _emit_multiple( q as emitter, p0 as p3, v0_parent as v3 )
            if i_active >= ubound(a_emi) then exit sub
      
            ''   with inaccurate f_velnorm() formula .. :\ ..
            '' attempt to convert velocity/(frame) to velocity/(1 sec)
            dim as single fps      = 1 / q._one_over_fps
            dim as v3     v0_norm = f_velnorm( v0_parent, q._k, fps)
            dim as v3     a0_norm = f_velnorm( q.accel, q._k, fps)
            
            for j as long = 1 to int( q._one_over_fps * q._pps + rnd)
                static as v3 v_explo : v_explo.rand_on_sphere( spawn_vel + rnd * spawn_dv )
                dim as single density = .3 + .05 * rnd
                dim as single life = 1.5 + rnd
                _age = rnd
                new_particle p0, v_explo, density, fps, life, _age, v0_norm, a0_norm
                dim byref as emitter e = a_emi(i_active)
                e.pos += (q.vel) * (1-_age)  + v_explo * ( _age) '' close enough :P
                if i_active >= ubound(a_emi) then exit for
            next
        end sub
        
        function f_recycle( e as emitter ) as boolean
            if e.life <= 0 then
                e = a_emi(i_active) '' replace with highest element
                i_active -= 1       '' reduce stack pointer
                return true
            else
                return false
            endif
        end function
    
    sub set_emission( e as emitter, pps as single, t as single )
        e._pps = -abs(pps) '' initial condition for _frame0_spawns()
        e._t_emit = t
    end sub

    sub phys_frame( e as emitter ) '' fast approxximate terminal velocity.  sometimes i reorder
        e.vel += e.accel
        e.vel *= e._k
        e.pos += e.vel
    end sub
    
    sub accel_timer( e as emitter, dt as single )
        e._t_accel -= dt
        e.accel += e.accel * ( e._t_accel <= 0 )
    End Sub
    
    sub emission_timer( e as emitter, dt as single )
        e._t_emit -= dt
        e._pps += e._pps * ( e._t_emit <= 0 )
    End Sub
    
    function f_phys_emit_and_recycle( e as emitter, dt as single ) as boolean
        while dt > 0
            if f_recycle( e ) then return true
            
            dim as p3 p0 = e.pos
            dim as v3 v0 = e.vel
            phys_frame e
            
            _emit_multiple e, p0, v0
            
            dim as single d = min( dt, e._one_over_fps )
            dt     -= d
            e.life -= d
            accel_timer e, d
            emission_timer e, d
            
            e._t_phys -= e._one_over_fps
        wend
        return false
    End function
    
        sub _frame0_spawns( e as emitter )
            if e._pps < 0 then
                e._pps = -e._pps
                _emit_multiple e, e.pos, e.vel
            endif
        end sub
        
        function frame_tick( e as emitter, t_zero as single ) as boolean
            return t_zero <= e._t_phys
        end function
    
    sub _rate_and_indexing_manager( e as emitter, dt as single, byref i as long )
        _frame0_spawns e
        
        dim as single t_zero = e.life - dt
        if frame_tick( e, t_zero ) then
            if f_phys_emit_and_recycle( e, dt ) then exit sub '' recycle pops high elem emitter to 'i'
        else
            e.life = t_zero
            accel_timer e, dt '' 2025 Feb 5
            emission_timer e, dt '' 2025 Feb 5
            if f_recycle( e ) then exit sub
        endif
        
        i += 1
    end sub

sub update_particles( dt as single )
    dim as long i
        while i <= i_active
    _rate_and_indexing_manager a_emi(i), dt, i
    wend
end sub
'
' -------- freebasic_particles_FX


    sub draw_particles
        for i as long = 0 to i_active
            dim byref as emitter e = a_emi(i)
            pset (e.pos.x, e.pos.y), rgb(255,255,255)
        next
    end sub

    function round(in as double, places as ubyte = 2) as string '' mostly for debug / print
      dim as integer mul = 10 ^ places
      return str(csng( int(in * mul + .5) / mul) )
    End Function


dim as long  w = 800
dim as long  h = 600

screenres w,h, 32

    '' -- scenario
    ''
dim as long   y_ = h - 50

dim as long   u = 4

type my_rocket_type
    as emitter  e
end type

dim as string str_fps_array(u)
dim as long   rocket_x_array(u)

dim as single size = sqr(w*w + h*h) / 10
dim as single life = 8
dim as single dens = 0.4

dim as v3     v = type(0,1,0) * -h / 15

for i as long = 0 to u

    dim as single fps = (i + .1) * 10

    str_fps_array(i) = round(fps)  '' printout
    rocket_x_array(i) = 50 + 150*i
    
    new_particle type( rocket_x_array(i), y_ - 2), v*0, dens, fps, life
    
    dim byref as emitter e = a_emi(i)
    
    set_accel e, v*1.5, 4.75               '' duration
        
        var particles_per_second = 999
        
    set_emission e, particles_per_second, 5.25 '' duration
    
next
    spawn_vel = ( .15 * (1 + 0)) * size
    spawn_dv = (.01) * size

    dim as double t = timer, tp = t, t1 = t + 10
    dim as double t_info_update = t + .5
''
''  scenario


while t < t1

    screenlock
    cls
    draw_particles
    
    line (1,y_)-(w-2,y_)
    draw string (5, y_ + 10), " FPS"
    for i as long = 0 to u
        draw string ( rocket_x_array(i), y_ + 10 ), round( 1/a_emi(i)._one_over_fps, 1 )
    next
    screenunlock
    
    t = timer
    update_particles t - tp '' a main sub
    tp = t

    if t > t_info_update then
        t_info_update += 2
    endif
    
    var kstr = inkey
    if kstr <> "" or i_active < 0 then exit while
    sleep 1

wend

locate 3,2
print "Done!"

sleep 2000

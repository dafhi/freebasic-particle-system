/' -- freebasic particle FX framework - 2025 Jan 27.u2 - by dafhi

    License:  use, modify, profit
    
      - features

    set acceleration or emission at any time
    vector math

    - update
    
    _age formula

    - will likely change

    _emit_multiple() subroutine, created in a demo-ish way
    set_accel algorithm (far-off future unless my IQ suddenly jumps)

'/

    const tau = 8 * Atn(1)

type p3 as v3

type v3
    declare sub       rand_on_sphere( as single = 1 )
    declare operator  cast as string

    as single         x,y,z
End Type

operator v3.cast as string
    return "x y z " + str(x) + " " + str(y) + " " + str(z)
end operator

sub v3.rand_on_sphere( f as single ):  y = 2*(rnd-.5) : var r = f*sqr(1-y*y)
  z=rnd*tau : x=r*cos(z) : z=r*sin(z) : y *= f
End Sub

operator -( byref L as p3, byref R as p3 ) as v3 : return type( L.x-R.x, L.y-r.y, L.z-R.z ) : end operator
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

        function round(in as double, places as ubyte = 2) as string '' mostly for debug / print
          dim as integer mul = 10 ^ places
          return str(csng( int(in * mul + .5) / mul) )
        End Function

    function geometric_sum( k as single = .5, n as single = 2) as double
        return (k-k^(n+1)) / (1-k)
    end function


type emitter
    
    as p3       pos           '' basic particle properties
    as v3       vel
    as single   life
    
    as v3       accel         '' spice
    
    as single   _k            '' fixed-rate density precalc
    as single   _one_over_fps
    
    as single   _t_phys       '' if life <= this, do physics
    
    as single   _pps          '' particles per sec
    as single   _t_accel      '' this <= 0, stop acceleration
    as single   _t_emit       '' this <= 0, stop emission

end type

    dim shared as emitter  a_emi(299999)
    dim shared as long     i_active = -1

        dim shared as single   spawn_vel = 50 '' demo variables
        dim shared as single   spawn_dv = 25

        dim shared as single _age, one_over_gsum
        dim shared as v3     gv_parent


    sub k_transfer( e as emitter, v_explo as v3, v_parent as v3, k as single, fps as single )
            one_over_gsum = 1 / geometric_sum( e._k, fps )
            gv_parent = v_parent * one_over_gsum
        
        e.vel = gv_parent + v_explo
        
        e.vel *= one_over_gsum       ' accurate - 2025 Jan 23
        'e.vel *= e._one_over_fps     ' approximate
    end sub


sub new_particle( p0 as v3, v_explo as v3, particle_density as single, physics_fps as single, life as single = 1, _v_parent as v3 = type(0,0,0) )
    
    if i_active >= ubound(a_emi) then exit sub
    
    i_active += 1
    dim byref as emitter e = a_emi(i_active)
    
    e._one_over_fps = 1 / physics_fps
    e._k = particle_density ^ e._one_over_fps
    
    k_transfer e, v_explo, _v_parent, particle_density, physics_fps
    e.pos = p0
    _age = e._k ^ rnd
    e.life = life - _age
    e._t_phys = e.life - e._one_over_fps       ' physics frame trigger
    
end sub
    
    sub set_accel( e as emitter, a as v3, t as single )
        dim as single fps = 1 / e._one_over_fps
        dim as single k = e._k ^ fps
        e.accel = a * e._one_over_fps ^ 2 / k
        e._t_accel = t
    end sub
          
        sub _emit_multiple( e as emitter, dt as single, p0 as p3, byval vel_parent as v3 )
            dim as single phys_fps = 1 / e._one_over_fps
            
            for j as long = 1 to int( dt * e._pps + rnd)
                
                static as v3 v_explo :  v_explo.rand_on_sphere( spawn_vel + rnd * spawn_dv )
                
                dim as single density = .3 + .05 * rnd
                dim as single life = 1.5 + rnd
                
                new_particle p0, v_explo, density, phys_fps, life, vel_parent
                a_emi(i_active).pos += (gv_parent + v_explo) * (_age * a_emi(i_active)._k)
                
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
        if e._t_accel <= 0 then
            e.accel = type(0,0,0)
            e._t_accel = 0
        EndIf
    End Sub
    
    sub emission_timer( e as emitter, dt as single )
        e._t_emit -= dt
        if e._t_emit <= 0 then e._pps = 0
    End Sub
    
    function f_phys_emit_and_recycle( e as emitter, dt as single ) as boolean
        while dt > 0
            e.life -= e._one_over_fps
            if f_recycle( e ) then return true

            phys_frame e
            _emit_multiple e, e._one_over_fps, e.pos, e.vel
            
            accel_timer e, e._one_over_fps
            emission_timer e, e._one_over_fps
            
            e._t_phys -= e._one_over_fps
            dt        -= e._one_over_fps
        wend
        return false
    End function
    
        sub _frame0_spawns( e as emitter )
            if e._pps < 0 then
                e._pps = -e._pps
                _emit_multiple e, e._one_over_fps, e.pos, (e.vel+e.accel) * e._k / 2
            endif
        end sub
        
        function frame_tick( e as emitter, t_zero as single ) as boolean
            return e._t_phys >= t_zero
        end function
    
    sub _rate_and_indexing_manager( e as emitter, dt as single, byref i as long )
        
        _frame0_spawns e '' 2025 Jan 27
        
        dim as single t_zero = e.life - dt
        if frame_tick( e, t_zero ) then
            if f_phys_emit_and_recycle( e, dt ) then exit sub '' recycle pops high elem emitter to 'i'
            e.life = t_zero
        else
            e.life = t_zero
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

' ----------------------

sub draw_particles
    for i as long = 0 to i_active
      pset ( a_emi(i).pos.x, a_emi(i).pos.y )
    next
end sub


dim as long  w = 800
dim as long  h = 600

screenres w,h, 32


dim as long   y_ = h - 50

dim as long   u = 4
dim as string str_fps_array(u)
dim as long   rocket_x_array(u)


dim as single size = sqr(w*w + h*h) / 10
dim as single life = 8
dim as single dens = 0.3

dim as v3     v = type(0,1,0) * -h / 15


    '' rockets
for i as long = 0 to u

    dim as single fps = (i+.2) * 10
    
    str_fps_array(i) = round(fps)  '' printout
    rocket_x_array(i) = 50 + 150*i
    
    '' a main sub
    new_particle type( rocket_x_array(i), y_ - 2), v*1, dens, fps, life
    
    dim byref as emitter e = a_emi(i)
    
    '' secondary sub
    set_accel e, v*3, 1.1               '' duration
        
        var particles_per_second = 299
        
    '' secondary sub
    set_emission e, particles_per_second, 5 '' duration
    
    spawn_vel = (.3) * size
    spawn_dv = (.01) * size
next

    dim as double t = timer, tp = t, t1 = t + 10
    dim as double t_info_update = t + .5

while t < t1

'    screenlock
    
    cls
    draw_particles
    
    line (1,y_)-(w-2,y_)
    draw string (5, y_ + 10), " FPS"
    for i as long = 0 to u
        draw string ( rocket_x_array(i), y_ + 10 ), str_fps_array(i)
    next
    
'    screenunlock

    t = timer
    update_particles t - tp '' a main sub
    tp = t

    if t > t_info_update then
        t_info_update += 2
    endif
    
    var kstr = inkey
    if kstr <> "" or i_active < 0 then exit while
    sleep 15

wend


locate 3,2
print "Done!"

sleep 2000

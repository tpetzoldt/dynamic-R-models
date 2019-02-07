c file lorenz.f
       subroutine initmod(odeparms)
         external odeparms
         double precision parms(3)
         common /myparms/parms

         call odeparms(3, parms)
         return
       end

       subroutine derivs (neq, t, y, ydot, yout, ip)
         double precision t, y, ydot, a, b, c
         integer neq, ip(*)
         dimension y(3), ydot(3), yout(*)
         common /myparms/a,b,c

         ydot(1) = a * y(1) + y(2) * y(3)
         ydot(2) = b * (y(2) - y(3))
         ydot(3) = -y(1) * y(2) + c * y(2) - y(3)

        return
       end

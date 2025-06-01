       identification division.
       program-id. insertionsort.
       environment division.
       configuration section.
       repository. function all intrinsic.
       data division.
       working-storage section.
       01  filler.
           03  a pic 9(5).
           03  a-lim pic 9(5) value 10000.
           03  array occurs 10000 pic 9(5).
       
       01  filler.
           03  s pic 9(5).
           03  o pic 9(5).
           03  o1 pic 9(5).
           03  sorted-len pic 9(5).
           03  sorted-lim pic 9(5) value 10000.
           03  sorted-array occurs 10000 pic 9(5).
       
       procedure division.
       start-insertionsort.
           move 1 to a 
           perform varying a from 1 by 1 until a > a-lim
               move a  to  array(a)
           end-perform
       
      *      perform varying a from 1 by 1 until a > a-lim
      *          display space array(a) with no advancing
      *     end-perform
      *     display  space 'initial array'
      *    
           
           call "measure_energy"

           move 0 to sorted-len
           perform varying a from 1 by 1 until a > a-lim
               perform varying s from 1 by 1
               until s > sorted-len
               or array(a) <= sorted-array(s)
                   continue
               end-perform
       
               perform varying o from sorted-len by -1
               until o < s
                   compute o1 = o + 1
                   move sorted-array(o) to sorted-array(o1)
               end-perform
       
               move array(a) to sorted-array(s)
       
               add 1 to sorted-len
           end-perform

           call "measure_energy"
       
      *     perform varying s from 1 by 1 until s > sorted-lim
      *        display space sorted-array(s) with no advancing
      *    end-perform
      *    display space 'sorted array'
       
           stop run
           .
       end program insertionsort.
       

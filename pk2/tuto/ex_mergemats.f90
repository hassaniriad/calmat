   program ex_mergemats
      use pk2mod_m

      ex1: block
         type(pk2_t) :: s1, s2, s3, s4, s5, a
         integer     :: i

         s1 = reshape([11,21,31,12,22,32,13,23,33]*1_Ikind,[3,3])
         s2 = reshape([14,24,34]*1_Ikind,[3,1])
         s3 = reshape([15,25,35,16,26,36]*1_Ikind,[3,2])
         s4 = reshape([41,42,43]*1_Ikind,[1,3])
         s5 = reshape([44,45,46]*1_Ikind,[1,3])

         call s1%printMe(msg='1) Let s1 = ',form='values')  
         call s2%printMe(msg='   and s2 = ',form='values')  
         call s3%printMe(msg='   and s3 = ',form='values')  
         call s4%printMe(msg='   and s4 = ',form='values')  
         call s5%printMe(msg='   and s5 = ',form='values')

         ! Form the matrix [s1,s2,s3 ; s4,s5]:

         a = mergemats( [s1,s2,s3,s4,s5], delim = ",,;," )
         ! or also: call s_mergemats( [s1,s2,s3,s4,s5], delim = ",,;,", a )

         call a%printMe(msg='then [s1,s2,s3 ; s4,s5] = ',form='values')  

         s1 = reshape([('ifort',i=1,9)],[3,3])
         s2 = reshape([('gfortran',i=1,3)],[3,1])
         s3 = reshape([('nagfor',i=1,4)],[1,4])

         call s1%printMe(msg='2) Let s1 = ',form='values')  
         call s3%printMe(msg='   and s2 = ',form='values')
         call s3%printMe(msg='   and s3 = ',form='values')

         a = mergemats( [s1,s2,s3], delim = ',;')

         call a%printMe(msg='then [s1,s2 ; s3] = ',form='values')
      end block ex1

      ex2: block
         ! same but with an array of pk2_t
         type(pk2_t) :: allS(3), a
         integer     :: i

         allS(1) = reshape([('ifort',i=1,9)],[3,3])
         allS(2) = reshape([('gfortran',i=1,3)],[3,1])
         allS(3) = reshape([('nagfor',i=1,4)],[1,4])

         a = mergemats( allS, delim = ',;')

      end block ex2
      
   end program ex_mergemats

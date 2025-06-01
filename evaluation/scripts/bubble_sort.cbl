       identification division.
       program-id. BUBBLSRT.
       data division.
       working-storage section.
       01 changed-flag      pic x.
          88 hasChanged         value 'Y'.
          88 hasNOTChanged      value 'N'.
       01 itemCount         pic 9(5).
       01 tempItem          pic 9(5).
       01 itemArray.   
          03 itemArrayCount pic 9(5).
          03 item           pic 9(5) occurs 99999 times
                                   indexed by itemIndex.
      *          
       procedure division.
       main.
      * place the values to sort into itemArray
           move 99999 to itemArrayCount
           perform varying itemIndex from 1 by 1
               until itemIndex > itemArrayCount
               move itemIndex to item(itemIndex)
           end-perform
      * store the starting count in itemCount and perform the sort    
           move itemArrayCount to itemCount
           
           call "measure_energy"    
           perform bubble-sort
           call "measure_energy"
           


      * output the results     
      *     perform varying itemIndex from 1 by 1 
      *        until itemIndex > itemArrayCount
      *        display item (itemIndex) ';' with no advancing
      *     end-perform   
      * thats it!       
           stop run.
      *     
       bubble-sort.
           perform with test after until hasNOTchanged
              set hasNOTChanged to true
              subtract 1 from itemCount
              perform varying itemIndex from 1 by 1 
                 until itemIndex > itemCount
                 if item (itemIndex) > item (itemIndex + 1)
                    move item (itemIndex) to tempItem
                    move item (itemIndex + 1) to item (itemIndex)
                    move tempItem to item (itemIndex + 1)
                    set hasChanged to true
                 end-if   
              end-perform   
           end-perform   

       exit program.

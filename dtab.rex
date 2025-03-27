/* decision_table.rexx */

parse arg infile .
if infile = "" 
then do
  say "ERROR: please specify your DTAB as parameter!"
  say "FORMAT: >regina full-qualified-DTAB-file-name"
  exit
end  

say "INPUT: " infile   

/* Initialize arrays */
condMatrix. = ''
condStub. = ''
actMatrix. = ''
actStub. = ''

call parseTable infile
exit

/* Decision Table to COBOL Transpiler */ 
parseTable: procedure expose condMatrix. condStub. actMatrix. actStub.
  parse arg filename
  insideTable = 0
  condPart = 1
  condRow = 0
  actRow = 0
  colCount = 0


  say '* Decision table: ========================' 
  /* Read input file */
  do while lines(filename) > 0
    line = linein(filename)
	say line
    clean = strip(line)
    /*say "DEBUG: Read line ->" clean*/

    /* Detect table start */
    if pos("DECISION_TABLE", clean) > 0 then do
      insideTable = 1
      /*say "DEBUG: Found DECISION_TABLE"*/
      iterate
    end

    /* Detect table end */
    if pos("END_TABLE", clean) > 0 then do
      /*say "DEBUG: Found END_TABLE"*/
      leave
    end

    if insideTable then do
      /* Skip separator lines */
      if pos('-----', clean) > 0 then do
        /*say "DEBUG: Found separator line"*/
        condPart = 0
        iterate
      end

      /* Remove "//" if it's at the start */
      if left(clean, 2) = '//' 
	  then 
	    clean = strip(substr(clean, 3))

      /* Split condition/action part */
      parse var clean entries '|' stub
      entries = strip(entries)
      stub = strip(stub)

      if condPart then do
        /* Parsing condition row */
        condRow = condRow + 1
        condStub.condRow = stub

        /* Ensure column count is set */
        if condRow = 1 then do
          colCount = words(entries)
          condMatrix.1 = colCount
          /*say "DEBUG: colCount set to" colCount*/
        end

        /* Store conditions in matrix */
        do i = 1 to colCount
          condMatrix.condRow.i = word(entries, i)
          /*say "DEBUG: Condition["condRow","i"] ->" condMatrix.condRow.i*/
        end
      end
      else do
        /* Parsing action row */
        actRow = actRow + 1
        actStub.actRow = stub

        if actRow = 1 then actMatrix.1 = colCount

        /* Store actions in matrix */
        do i = 1 to colCount
          actMatrix.actRow.i = word(entries, i)
          /*say "DEBUG: Action["actRow","i"] ->" actMatrix.actRow.i*/
        end
      end
    end
  end

  /* Debugging summary */
  /*say "DEBUG: Parsing complete. condRow=" condRow ", actRow=" actRow ", colCount=" colCount*/
  
 say ' '
 say '* Generated code: ========================'

  /* Error handling */
  if condRow = 0 | actRow = 0 then do
    say "ERROR: No valid conditions or actions found in the decision table!"
    exit 1
  end
  
  /*

  say 'DEBUG: Parsed Conditions:'
  do row = 1 to condRow
    say 'Row' row ':'
    do col = 1 to colCount
        say '  Condition['row','col'] ->' condMatrix.row.col
    end
  end

  say 'DEBUG: Parsed Actions:'
  do row = 1 to actRow
    say 'Row' row ':'
    do col = 1 to colCount
        say '  Action['row','col'] ->' actMatrix.row.col
    end
  end
  
  */

  /* Generate COBOL */
  call generateCobol condRow, actRow, colCount /*, condMatrix., condStub., actMatrix., actStub.*/
  return

/* COBOL Code Generation */
/* COBOL Code Generation */
generateCobol: procedure expose condMatrix. condStub. actMatrix. actStub.
  parse arg condRow, actRow, colCount

  /*say "DEBUG: Entering generateCobol with condRow=" condRow ", actRow=" actRow ", colCount=" colCount*/

  if condRow = 0 | actRow = 0 then do
    say "ERROR: No valid data received in generateCobol!"
    exit 1
  end

  say "EVALUATE TRUE"
  
  /*say 'DEBUG: Generating COBOL code from parsed table:'*/
  do col = 1 to colCount
    condExpr = ''  /* Reset condition expression */
    
    do row = 1 to condRow
        if condMatrix.row.col = 'N' then condExpr = condExpr "NOT (" condStub.row ") AND"
        if condMatrix.row.col = 'Y' then condExpr = condExpr "(" condStub.row ") AND"
    end

    if condExpr \= '' then do
        condExpr = substr(condExpr, 1, length(condExpr) - 4) /* Remove last 'AND' */
        say '  WHEN (' condExpr ')'

        /* Process actions */
        do actRowIndex = 1 to actRow
            if actMatrix.actRowIndex.col = 'X' then do
                say '       ' actStub.actRowIndex  
            end
        end
    end
  end

  say "  WHEN OTHER"
  say "       CONTINUE"
  say "END-EVALUATE."
return  

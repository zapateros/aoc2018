 inp = '157901'
 r1 = 0
 r2 = 1
 rec = '37'
 while inp not in rec[-7:]:
     rec += str(int(rec[r1]) + int(rec[r2]))
     r1 =(int(rec[r1])+1+r1)%len(rec)
     r2 =(int(rec[r2])+1+r2)%len(rec)
 
 print( rec.index(inp))

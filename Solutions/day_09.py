# This solution is made in Python because the calculation in R is taking far too long
# Even this solution in Python takes up to a couple of hours, but hey.. it works!
# I later learned about linked lists and double ended queues. This latter one should work well in this case (collection: deque)

from datetime import datetime

i=1
r=[0,1]
n=1
mb=2
cnt=0
players=418
pl=[0]*players

aoc91 = 71339 
aoc92 = 7133900 

while mb < aoc92: #change to aoc91 for 9.1
    
    i += 1

    if i == (players + 1):
        i = 1
        
    if round( mb / 23) == (mb/23):
        if n>8 :
            j= n-8
            sm1 = r[j-1]
            del r[j-1]
            n = j+1
            mb +=1
        else:
            j =  len(r) - (8-n)
            sm1 = r[j-1]
            del r[j-1]
            n = j+1

            if j > len(r):
                n=2

            mb += 1

        pl[i-1] +=sm1 + mb - 1
    else:
        if len(r) == n:
            n = 1
            r.append(mb)
            mb += 1
        else:
            if n>len(r) :
                n = 1
            r.insert(n,mb)
            mb +=1
            n += 2
    if(round( mb / 50000) == (mb/50000)):
        print(datetime.now().strftime('%Y-%m-%d %H:%M:%S'), mb)

print(max(pl))

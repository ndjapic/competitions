# Problem: B.pas

```pascal
program Fill_the_Gaps;
var
   n, i, a1, a2, x: int8;

begin
     readln(n);
     read(a1);
     write(a1);
     for i := 2 to n do begin
         read(a2);
         if a2 > a1 then
            for x := a1+1 to a2 do write(' ', x)
         else
           for x := a1-1 downto a2 do write(' ', x);
         a1 := a2;
     end;
     readln;
end.


```

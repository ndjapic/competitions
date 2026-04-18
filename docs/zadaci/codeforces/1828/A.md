# Задатак: A.pas

```pascal
program Divisible_Array;
var
    notc, n, i: int16;
    a: array [1 .. 200] of int16;

begin
    readln(notc);
    repeat

      readln(n);

      for i := 1 to n do a[i] := i;

      if not odd(n) then inc(a[1], n div 2);

      for i := 1 to n-1 do write(a[i], ' ');
      writeln(a[n]);

      dec(notc);
    until notc = 0;
end.


```

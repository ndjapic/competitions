# Problem: A_Brick_Wall.pas

```pascal
program A_Brick_Wall;
uses
	math;
var
    ntc, tci, n, m, ans: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n, m);
		ans := m div 2 * n;
		writeln(ans);

    end;
end.

```

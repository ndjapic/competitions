# Задатак: A_Square_of_Rectangles.pas

```pascal
program A_Square_of_Rectangles;
{$MODE DELPHI}
var
	ntc, tci, l1, b1, l2, b2, l3, b3: int32;

function solution(l1, b1, l2, b2, l3, b3: int32): boolean;
var
	cond1, cond2: boolean;
begin
	cond1 := (l1+l2+l3 = b1) and (b1 = b2) and (b2 = b3);
	cond2 := (l1+l2 = b1) and (l2 = l3) and (b1 = b2+b3);
	result := cond1 or cond2;
end;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(l1, b1, l2, b2, l3, b3);

		if solution(l1, b1, l2, b2, l3, b3) then
			writeln('YES')
		else if solution(b1, l1, b2, l2, b3, l3) then
			writeln('YES')
		else
			writeln('NO');

	end;
end.

```

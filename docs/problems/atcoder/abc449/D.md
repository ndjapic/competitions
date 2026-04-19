# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	xx = 1000 * 1000;
var
    l, r, d, u: int32;

function first(x, y: int32): int64;
var
	h: int32;
begin
	h := x div 2;
	if x < y then
		first := first(y, x)
	else if y <= 0 then
		first := 0
	else if x = y then
		first := int64(h+1) * h
	else
		first := first(y, y) + int64(x-y + y mod 2) div 2 * y;
end;

function axis(x: int32): int32;
begin
	if x <= 0 then
		axis := 0
	else
		axis := x div 2;
end;

function f2(x, y: int32): int64;
begin
	if x < y then
		f2 := f2(y, x)
	else if y >= 0 then
		f2 := first(x, y)
			+ first(xx, y)
			+ first(x, xx)
			+ first(xx, xx)
			+ axis(x)
			+ axis(y)
			+ axis(xx) * 2 + 1
	else if x < 0 then
		f2 := first(xx, xx)
			- first(-x-1, xx)
			- first(xx, -y-1)
			+ first(-x-1, -y-1)
	else
		f2 := first(x, xx)
			- first(x, -y-1)
			+ first(xx, xx)
			- first(xx, -y-1)
			+ axis(xx)
			- axis(-y-1);
end;

begin
    readln(l, r, d, u);
	writeln(f2(r, u) - f2(r, d-1) - f2(l-1, u) + f2(l-1, d-1));
end.

```

# Problem: C_Count_Good_Numbers.pas

```pascal
program C_Count_Good_Numbers;
{$MODE DELPHI}
var
	ntc, tci: int32;
	l, r: int64;

function ans(r: int64): int64;
begin
	result := r;

	dec(result, r div 2);
	dec(result, r div 3);
	dec(result, r div 5);
	dec(result, r div 7);

	inc(result, r div (2*3));
	inc(result, r div (2*5));
	inc(result, r div (2*7));
	inc(result, r div (3*5));
	inc(result, r div (3*7));
	inc(result, r div (5*7));

	dec(result, r div (2*3*5));
	dec(result, r div (2*3*7));
	dec(result, r div (2*5*7));
	dec(result, r div (3*5*7));

	inc(result, r div (2*3*5*7));
end;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(l, r);
		writeln(ans(r) - ans(l-1));

	end;
end.

```

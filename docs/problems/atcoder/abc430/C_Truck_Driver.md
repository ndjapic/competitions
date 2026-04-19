# Problem: C_Truck_Driver.pas

```pascal
program C_Truck_Driver;
{$MODE DELPHI}
uses
	math;
const
	nn = 300 * 1000;
	nn2 = nn * nn;
var
	n, a, b, i, la, lb, r: int32;
	ans: int64;
	s: string;
	ca, cb: array [0 .. nn] of int32;

begin
	readln(n, a, b);
	readln(s);

	ca[0] := 0;
	cb[0] := 0;

	for i := 1 to n do begin
		ca[i] := ca[i-1];
		cb[i] := cb[i-1];
		case s[i] of
			'a': inc(ca[i]);
			'b': inc(cb[i]);
		end;
	end;

	ans := 0;
	la := 1;
	lb := 1;

	for r := 1 to n do
		if ca[r] >= a then begin
			while (la <= r) and (ca[r] - ca[la] >= a) do inc(la);
			while (lb <= r) and (cb[r] - cb[lb-1] >= b) do inc(lb);
			inc(ans, max(0, la-lb+1));
		end;

	writeln(ans);
end.

```

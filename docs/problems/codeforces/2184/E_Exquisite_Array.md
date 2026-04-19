# Problem: E_Exquisite_Array.pas

```pascal
program E_Exquisite_Array;
uses
	math;
var
	notc, tci, n, k, d, r, ans: int32;

function ncr(n, r: int32): int32;
begin
	if r = 0 then
		ncr := 1
	else
		ncr := ncr(n-1, r-1) * n div r;
end;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);

		d := 0;
		ans := n;
		while 1 shl d <= n do begin
			for r := 0 to d do
				if r + d + 1 <= k then
					dec(ans, ncr(d, r));
			inc(d);
		end;

		writeln(max(ans, 0));

	end;
end.

```

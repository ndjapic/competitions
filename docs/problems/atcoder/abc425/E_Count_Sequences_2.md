# Problem: E_Count_Sequences_2.pas

```pascal
program E_Count_Sequences_2;
const
	nn = 5000;
var
	ntc, tci, m, n, i, r, c, s, ans: int32;
	ncr: array [0 .. nn, 0 .. nn] of int32;

begin
	readln(ntc, m);

	ncr[0, 0] := 1;
	for n := 1 to nn do begin
		ncr[n, 0] := 1;
		for r := 1 to n-1 do begin
			ncr[n, r] := ncr[n-1, r-1] + ncr[n-1, r];
			if ncr[n, r] >= m then dec(ncr[n, r], m);
		end;
		ncr[n, n] := 1;
	end;

	for tci := 1 to ntc do begin

		readln(n);

		s := 0;
		ans := 1;
		for i := 1 to n do begin
			read(c);
			inc(s, c);
			ans := int64(ans) * ncr[s, c] mod m;
		end;
		readln;

		writeln(ans);

	end;
end.

```

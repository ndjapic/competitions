# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, h, k: int32;
	ans: int64;
	a, mn, mx: array [1 .. nn] of int32;
	s: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, h, k);

		s[0] := 0;
		for i := 1 to n do begin
			read(a[i]);
			s[i] := s[i-1] + a[i];
		end;
		readln;

		mn[1] := a[1];
		mx[n] := a[n];
		for i := 2 to n do mn[i] := min(mn[i-1], a[i]);
		for i := n-1 downto 1 do mx[i] := max(mx[i+1], a[i]);

		ans := int64(h) div s[n] * (n+k);
		h := h mod s[n];

		if h = 0 then
			dec(ans, k)
		else begin
			i := 1;
			while (i < n) and (s[i] < h) and (s[i] - mn[i] + mx[i+1] < h) do inc(i);
			inc(ans, i);
		end;
		writeln(ans);

	end;
end.

```

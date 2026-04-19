# Problem: D.pas

```pascal
program _D;
const
	nn = 100 * 1000;
var
	n, k, i, i0, t: int32;
	l, r, m: int64;
	a: array [1 .. nn] of int32;
	s: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(n, k);

	s[0] := 0;
	for i := 1 to n do begin
		read(a[i]);
		s[i] := s[i-1] + a[i];
	end;
	readln;

	l := 1;
	r := l shl 60;
	while r-l > 1 do begin
		m := (l+r) div 2;
		t := 0;
		i0 := 0;

		for i := 1 to n do
			if s[i] - s[i0] >= m then begin
				inc(t);
				i0 := i;
			end;

		if t < k then
			r := m
		else
			l := m;
	end;

	writeln(l);
end.

```

program B_Bird_Watching;
const
	nn = 100;
var
	n, m, i, a, b: int32;
	c, s: array [1 .. nn] of int32;

begin
	readln(n, m);

	for a := 1 to m do begin
		c[a] := 0;
		s[a] := 0;
	end;

	for i := 1 to n do begin
		readln(a, b);
		inc(c[a]);
		inc(s[a], b)
	end;

	for a := 1 to m do writeln(s[a] / c[a]:0:6);
end.

program _B;
const
	nn = 100 * 1000;
var
	n, m, k, i, j, c: int32;
	s: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(n, m, k);

	for i := 1 to n do read(s[i]);
	readln;

	for j := 1 to m do begin
		read(i);
		readln(s[i]);
	end;

	c := 0;
	for i := 1 to n do
		if s[i] < k then inc(c);

	writeln(c);
end.

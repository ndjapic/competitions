program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 1000 * 1000;
var
	n, i, j, k: int32;
	ans: int64;
	s: string;
	a: array [1 .. NN] of record
		ch: char;
		c: int32;
	end;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	k := 1;
	a[1].ch := s[1];
	a[1].c := 1;

	for i := 2 to n do
		if s[i] = a[k].ch then
			inc(a[k].c)
		else begin
			inc(k);
			a[k].ch := s[i];
			a[k].c := 1;
		end;

	ans := 0;
	for j := 3 to k do
		if a[j-2].ch = a[j].ch then
			inc(ans, int64(a[j-2].c) * a[j].c);

	writeln(ans);
end.

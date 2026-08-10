program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #WA
uses
	math;
const
	NN = 100;
var
	n, i, j, j0, t, ans: int32;
	ch: char;
	s: string;
	col: array [1 .. NN, '0' .. '9'] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := 1;
	for i := 1 to n do begin
		readln(s);
		for j := 1 to 10 do
			col[i, s[j]] := j;
	end;

	ans := 10 * n;
	for ch := '0' to '9' do begin
		j0 := 1;
		t := 0;

		for i := 1 to n do begin
			j := col[i, ch];
			inc(t, j - j0);
			if j <= j0 then inc(t, 10);
			j0 := j;
		end;

		ans := min(ans, t);
	end;

	writeln(ans);
end.

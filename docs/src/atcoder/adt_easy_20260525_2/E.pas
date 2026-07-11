program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 10;
var
	n, m, i, j, c, a: int8;
	ans: int16;
	s: array [1 .. NN] of int16;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(i: int8; x: int16);
begin
	if i < m then begin
		inc(i);
		dfs(i, x);
		dfs(i, x or s[i]);
	end else if x+1 = 1 shl n then
		inc(ans);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to m do begin
		readln(c);
		s[i] := 0;
		for j := 1 to c do begin
			read(a);
			inc(s[i], 1 shl (a-1));
		end;
		readln;
	end;

	ans := 0;
	dfs(0, 0);
	writeln(ans);
end.

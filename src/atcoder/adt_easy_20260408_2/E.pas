program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 10;
var
	n, m: int8;
	a: array [0 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(i: int8);
var
	j: int8;
begin
	if i <= n then
		for j := a[i-1] + 1 to m do begin
			a[i] := j;
			dfs(i+1);
		end
	else begin
		for i := 1 to n-1 do write(a[i], ' ');
		writeln(a[n]);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);
	a[0] := 0;
	dfs(1);
end.

program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dfs
const
	NN = 8;
var
	n, k, i: int8;
	r, a: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(i, s: int8);
var
	j: int8;
begin
	inc(i);
	if i <= n then
		for j := 1 to r[i] do begin
			a[i] := j;
			dfs(i, s + j);
		end
	else if s mod k = 0 then begin
		for i := 1 to n-1 do write(a[i], ' ');
		writeln(a[n]);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do read(r[i]);
	readln;

	dfs(0, 0);
end.

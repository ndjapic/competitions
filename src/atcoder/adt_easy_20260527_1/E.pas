program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dfs #mod
var
	n, i, j, ans: int32;
	x: int64;
	a: array of array of int32;
	l: array of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(i: int32; p: int64);
var
	j: int32;
begin
	if i < n then begin
		for j := 0 to l[i] - 1 do
			if x div p mod a[i][j] = 0 then
				dfs(i+1, p * a[i][j]);
	end else if p = x then
		inc(ans);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, x);

	setlength(a, n);
	setlength(l, n);
	for i := 0 to n-1 do begin
		read(l[i]);
		setlength(a[i], l[i]);
		for j := 0 to l[i] - 1 do read(a[i][j]);
		readln;
	end;

	ans := 0;
	dfs(0, 1);
	writeln(ans);
end.

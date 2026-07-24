program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #graph #tree #dfs
uses
	Generics.Collections;
const
	NN = 500 * 1000;
var
	n, i, p: int32;
	ans: boolean;
	c: array [1 .. NN] of TList<int32>;
	v, s: array [1 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(p: int32);
var
	i: int32;
begin
	if ans then begin
		s[p] := 0;
		for i in c[p] do begin
			dfs(i);
			inc(s[p], v[i]);
		end;
		ans := ans and (v[p] >= s[p]);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do c[i] := TList<int32>.Create;
	try

		for i := 2 to n do begin
			read(p);
			c[p].Add(i);
		end;
		readln;

		for i := 1 to n do read(v[i]);
		readln;

		ans := true;
		dfs(1);

	finally
		for i := 1 to n do c[i].Free;
	end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.

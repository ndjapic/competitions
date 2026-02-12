program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults;
const
	nn = 15;
var
	n, m, i, j: int8;
	x: int32;
	w: TList<int32>;
	c: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function dfs(i: int8): boolean;
var
	j: int8;
	x: int32;
begin
	Result := i < 0;
	if not Result then x := w[i];
	for j := 1 to m do
		if not Result then begin
			dec(c[j], x);
			Result := (c[j] >= 0) and dfs(i-1);
			inc(c[j], x);
		end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	w := TList<int32>.Create;
	for i := 1 to n do begin
		read(x);
		w.Add(x);
	end;
	readln;
	w.Sort;

	for j := 1 to m do read(c[j]);
	readln;

	if dfs(n-1) then
		writeln('Yes')
	else
		writeln('No');

	w.Free;
end.

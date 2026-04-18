program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 8;
var
	n, m, i, a, b: int32;
	ans: boolean;
	adj1, adj2: array [1 .. nn, 1 .. nn] of boolean;
	p: array [1 .. nn] of int8;
	seen: array [1 .. nn] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure dfs(a: int8);
var
	b: int8;
begin
	if ans then
	else if a < n then begin
		inc(a);
		for b := 1 to n do
			if not seen[b] then begin
				p[a] := b;
				seen[b] := true;
				dfs(a);
				seen[b] := false;
			end;
	end else begin
		ans := true;
		for a := 1 to n do
			for b := 1 to n do
				ans := ans and (adj1[a, b] = adj2[p[a], p[b]]);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for a := 1 to n do
		for b := 1 to n do begin
			adj1[a, b] := false;
			adj2[a, b] := false;
		end;

	for i := 1 to m do begin
		readln(a, b);
		adj1[a, b] := true;
		adj1[b, a] := true;
	end;

	for i := 1 to m do begin
		readln(a, b);
		adj2[a, b] := true;
		adj2[b, a] := true;
	end;

	ans := false;
	for a := 1 to n do seen[a] := false;
	dfs(0);

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.

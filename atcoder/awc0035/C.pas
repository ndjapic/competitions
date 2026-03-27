program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections,
	Generics.Defaults;
const
	nn = 200 * 1000;
var
	n, m, i, u, v: int32;
	t: array [1 .. nn] of int32;
	adj: array [1 .. nn] of TList<int32>;
	blackout: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for v := 1 to n do begin
		read(t[v]);
		adj[v] := TList<int32>.Create;
	end;
	readln;

	for i := 1 to m do begin
		readln(u, v);
		adj[u].Add(v);
		dec(t[v]);
	end;

	blackout := TList<int32>.Create;
	for v := 1 to n do
		if t[v] < 0 then blackout.Add(v);

	i := 0;
	while i < blackout.Count do begin
		u := blackout[i];
		for v in adj[u] do
			if t[v] >= 0 then begin
				dec(t[v]);
				if t[v] < 0 then blackout.Add(v);
			end;
		inc(i);
	end;
	blackout.Sort;

	if blackout.Count > 0 then begin
		for i := 0 to blackout.Count - 2 do
			write(blackout[i], ' ');
		writeln(blackout[blackout.Count - 1]);
	end else
		writeln(-1);

	for v := 1 to n do adj[v].Free;
	blackout.Free;
end.

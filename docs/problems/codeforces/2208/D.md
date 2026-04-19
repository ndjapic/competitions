# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections;
const
	nn = 500;
var
	notc, tci, n, i, j, l: int32;
	ans: boolean;
	s: array [1 .. nn] of string;
	bfs: TList<int32>;
	seen: array [1 .. nn] of boolean;
	adj: array [1 .. nn] of TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do begin
			readln(s[i]);
			adj[i] := TList<int32>.Create;
		end;

		bfs := TList<int32>.Create;
		bfs.Add(1);
		seen[1] := true;
		ans := true;
		for l := 0 to n-1 do
			if ans then begin
				if l < bfs.Count then begin
					i := bfs[l];
					seen[i] := true;
					for j := 1 to n do
						if seen[j] then
						else if s[i][j] = '1' then begin
							adj[i].Add(j);
							bfs.Add(j);
							seen[j] := true;
						end else if s[j][i] = '1' then begin
							adj[j].Add(i);
							bfs.Add(j);
							seen[j] := true;
						end;
				end else
					ans := false;
			end;

		if ans then begin
			writeln('Yes');
			for i := 1 to n do
				for j in adj[i] do
					writeln(i, ' ', j);
		end else
			writeln('No');

		for i := 1 to n do adj[i].Free;
		bfs.Free;

	end;
end.

```

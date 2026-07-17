program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #bfs #pathfinder01 #queue
uses
	Generics.Collections;
const
	NN = 18;
var
	notc, tci, i, j, p2: int32;
	n: int8;
	s: string;
	bfs: TQueue<int32>;
	seen: array [0 .. 1 shl NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	bfs := TQueue<int32>.Create;

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		bfs.Clear;
		bfs.Enqueue(0);
		for i := 1 to 1 shl n do seen[i] := false;

		while bfs.Count > 0 do begin
			i := bfs.Dequeue;
			p2 := 1 shl n;
			while p2 > 1 do begin
				p2 := p2 div 2;
				j := i + p2;
				if not seen[j] and (i and p2 = 0) and (s[j] = '0') then begin
					seen[j] := true;
					bfs.Enqueue(j);
				end;
			end;
		end;

		if seen[(1 shl n) - 1] then
			writeln('Yes')
		else
			writeln('No');

	end;

	bfs.Free;
end.

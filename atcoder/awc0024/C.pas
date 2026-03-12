program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	hh = 2000;
var
	h, w, i, j: int32;
	g: array [1 .. hh] of string;
	ch: char;
	row, col: array [1 .. hh, 'a' .. 'z'] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	for ch := 'a' to 'z' do begin
		for i := 1 to h do row[i, ch] := 0;
		for j := 1 to w do col[j, ch] := 0;
	end;

	for i := 1 to h do begin
		readln(g[i]);
		for j := 1 to w do begin
			ch := g[i][j];
			inc(row[i][ch]);
			inc(col[j][ch]);
		end;
	end;

	for i := 1 to h do
		for j := 1 to w do begin
			ch := g[i][j];
			if (row[i][ch] = 1) and (col[j][ch] = 1) then write(ch);
		end;
	writeln;
end.

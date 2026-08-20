program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	h, w, i, j: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	for i := 1 to h do begin
		readln(s);
		for j := 2 to w do
			if (s[j-1] = 'T') and (s[j] = 'T') then begin
				s[j-1] := 'P';
				s[j] := 'C';
			end;
		writeln(s);
	end;
end.

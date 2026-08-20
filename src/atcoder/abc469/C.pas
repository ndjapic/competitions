program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 800 * 1000;
var
	n, k, i: int32;
	s: string;
	h, m: array [0 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	h[0] := 0;
	m[0] := 0;

	for i := 1 to N do begin
		h[i] := h[i-1];
		m[i] := m[i-1];
		case s[i] of
			'o': inc(h[i]);
			'x': inc(m[i]);
		end;
	end;

	i := 1;
	for k := 1 to n do begin
		while (i < n) and (m[i] - m[k] < h[k]) do inc(i);
		writeln(i);
	end;
end.

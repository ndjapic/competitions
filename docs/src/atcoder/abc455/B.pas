program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	hh = 10;
var
	h, w, h1, w1, h2, w2, i, j: int8;
	ans: int32;
	same: boolean;
	s: array [1 .. hh] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);
	for i := 1 to h do readln(s[i]);

	ans := 0;
	for h1 := 1 to h do
		for h2 := h1 to h do
			for w1 := 1 to w do
				for w2 := w1 to w do begin
					same := true;
					for i := h1 to h2 do
						for j := w1 to w2 do
							same := same and (s[i][j] = s[h1+h2-i][w1+w2-j]);
					if same then inc(ans);
				end;

	writeln(ans);
end.

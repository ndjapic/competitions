program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, m, i, j, k, ans: int32;
	w: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do read(w[i]);
	readln;

	for j := 1 to m do begin
		read(k);
		ans := 0;
		while k > 0 do begin
			dec(k);
			read(i);
			inc(ans, w[i]);
		end;
		readln;
		writeln(ans);
	end;
end.

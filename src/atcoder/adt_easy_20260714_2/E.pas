program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #bitset #mask #popcnt
const
	NN = 15;
	MM = 100;
var
	n, m, k, j, c, a: int8;
	x, ans: uint16;
	r: string;
	t: array [1 .. MM] of uint16;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);
	setlength(r, m);

	for j := 1 to m do begin
		t[j] := 0;
		read(c);

		while c > 0 do begin
			dec(c);
			read(a);
			inc(t[j], 1 shl (a-1));
		end;

		readln(r[j], r[j]);
	end;

	ans := 0;
	for x := 0 to (1 shl n) - 1 do begin
		j := 1;
		while (j <= m) and ((r[j] = 'o') = (PopCnt(x and t[j]) >= k)) do inc(j);
		if j > m then inc(ans);
	end;

	writeln(ans);
end.

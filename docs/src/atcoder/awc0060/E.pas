program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dag #parent #child #binary_lifting #ancestor
const
	nn = 100 * 1000;
	ee = 59;
var
	n, q, m, i, j, v: int32;
	k, p2: int64;
	e: int8;
	p: array [1 .. nn, 0 .. ee] of int32;
	d: array [1 .. nn, 0 .. ee] of int32;
	p10p2: array [0 .. ee] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q, m);

	for i := 1 to n do readln(d[i, 0], p[i, 0]);

	p10p2[0] := 10 mod m;
	for e := 0 to ee-1 do begin
		for i := 1 to n do begin
			p[i, e+1] := p[p[i, e], e];
			d[i, e+1] := (p10p2[e] * d[i, e] + d[p[i, e], e]) mod m;
		end;
		p10p2[e+1] := sqr(p10p2[e]) mod m;
	end;

	for j := 1 to q do begin
		readln(i, k);
		v := 0;
		e := 0;
		p2 := 1;

		while k >= p2 do begin
			if k and p2 > 0 then begin
				v := (p10p2[e] * v + d[i, e]) mod m;
				i := p[i, e];
			end;

			inc(e);
			inc(p2, p2);
		end;

		writeln(v);
	end;
end.

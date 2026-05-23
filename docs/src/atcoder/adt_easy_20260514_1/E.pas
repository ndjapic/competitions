program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, q, i, i0, j, c, l, r: int32;
	tp: int8;
	ans: int64;
	a: array [0 .. nn] of int32;
	s: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure modinc(var a: int32; b: int32);
begin
	inc(a, b);
	if a >= n then dec(a, n);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	s[0] := 0;
	for i := 0 to n-1 do begin
		read(a[i]);
		s[i+1] := s[i] + a[i];
	end;

	i0 := 0;
	for j := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				readln(c);
				modinc(i0, c);
			end;

			2: begin
				readln(l, r);
				modinc(l, i0 - 1);
				modinc(r, i0);
				ans := s[r] - s[l];
				if l >= r then inc(ans, s[n]);
				writeln(ans);
			end;

		end;
	end;
end.

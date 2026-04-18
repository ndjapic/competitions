program F;
{$OPTIMIZATION LEVEL3,ON}
const
	nn = 1 shl 18;
var
	notc, tci, n, q, i, i1, j, di, b, c, e, ans: int32;
	a: array [0 .. nn] of int32;
	t: array [0 .. 2*nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, q);
		di := (1 shl n) - 1;

		for i := 1 to 1 shl n do begin
			read(a[i]);
			t[i+di] := a[i];
		end;
		readln;

		for i := di downto 1 do t[i] := t[2*i+0] xor t[2*i+1];

		for j := 1 to q do begin

			readln(b, c);
			c := c xor a[b];
			inc(b, di);

			i := b;
			ans := 0;
			for e := 0 to n-1 do begin
				t[i] := t[i] xor c;
				i1 := i xor 1;
				if (t[i] < t[i1]) or (t[i] = t[i1]) and odd(i) then
					inc(ans, 1 shl e);
				i := i div 2;
			end;

			writeln(ans);

			i := b;
			for e := 0 to n-1 do begin
				t[i] := t[i] xor c;
				i := i div 2;
			end;

		end;

	end;
end.

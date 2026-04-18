program _C;
{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, n, m, i, j, x, y, s, h, c: int32;
	a: array of array of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, m);

		s := 0;
		setlength(a, n);
		for i := 0 to n-1 do begin
			setlength(a[i], m);
			for j := 0 to m-1 do begin
				read(a[i][j]);
				inc(s, a[i][j]);
			end;
			readln;
		end;
		h := s div 2;

		c := 0;
		i := n-1;
		j := 0;
		while c < h do begin
			inc(c, a[i][j]);
			inc(j);
			if j = m then begin
				dec(i);
				j := 0;
			end;
		end;

		writeln(int64(s-h) * h);
		for y := 1 to i do write('D');
		for x := 1 to j do write('R');
		write('D');
		for x := j+1 to m do write('R');
		for y := i+2 to n do write('D');
		writeln;

	end;
end.

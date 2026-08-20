program _E;
var
	n, a, b, p, q, r, s, x, y: int64;
	i, j: int32;
	ans: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, a, b);
	readln(p, q, r, s);

	setlength(ans, s-r+1);

	for i := 1 to q-p+1 do begin
		x := p+i-1;

		for j := 1 to s-r+1 do begin
			y := r+j-1;

			if (x-y = a-b) or (x+y = a+b) then
				ans[j] := '#'
			else
				ans[j] := '.';
		end;

		writeln(ans);
	end;
end.

program _C;
{$OPTIMIZATION LEVEL3,ON}
const
	nn = 50 * 1000;
var
	notc, tci: int32;
	n, i, j, k, d, x: int32;
	ch: char;
	found: boolean;
	ans: string;
	s: array [1 .. nn] of string;
	active: array [1 .. nn, 'a' .. 'z'] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);

		for i := 1 to n do
			for ch := 'a' to 'z' do active[i, ch] := false;

		for j := 1 to k do begin
			readln(s[j]);
			for i := 1 to n do active[i, s[j][i]] := true;
		end;

		d := 0;
		found := false;
		while (d < n) and not found do begin
			inc(d);
			if n mod d = 0 then begin
				setlength(ans, d);
				x := 1;
				found := false;

				while (x <= d) and not found do begin
					ans[x] := 'a';
					found := false;

					while (ans[x] <= 'z') and not found do begin
						i := x;
						while (i <= n) and active[i, ans[x]] do inc(i, d);
						found := i > n;
						if not found then inc(ans[x]);
					end;

					found := not found;
					inc(x);
				end;
				found := not found;
			end;
		end;

		for i := 1 to n div d do Write(ans);
		WriteLn;

	end;
end.

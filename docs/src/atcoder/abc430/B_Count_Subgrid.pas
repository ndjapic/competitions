program B_Count_Subgrid;
{$MODE DELPHI}
const
	nn = 10;
	nn2 = nn * nn;
var
	n, m, i, i1, j1, k1, i2, j2, k2, x: int32;
	s: array [1 .. nn] of string;
	t: array [1 .. nn2] of string;

begin
	readln(n, m);

	for i := 1 to n do readln(s[i]);

	k2 := 0;
	for i2 := m to n do
		for j2 := m to n do begin
			inc(k2);
			setlength(t[k2], m*m);
			x := 0;
			for i1 := i2-m+1 to i2 do
				for j1 := j2-m+1 to j2 do begin
					inc(x);
					t[k2][x] := s[i1][j1];
				end;

			k1 := 1;
			while t[k1] <> t[k2] do inc(k1);
			if k1 < k2 then begin
				{writeln('Found equal: ', k1, ' -> ', t[k1], ' and ', k2, ' -> ', t[k2]);}
				dec(k2);
			end;
		end;

	writeln(k2);
end.

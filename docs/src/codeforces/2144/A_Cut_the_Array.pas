program A_Cut_the_Array;
const
	nn = 40;
var
	notc, tci: int16;
	n, i, l, r, l0, r0, s1, s2, s3: int8;
	found: boolean;
	a: array [1 .. nn] of int8;
	s: array [0 .. nn] of int16;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		s[0] := 0;
		for i := 1 to n do begin
			read(a[i]);
			s[i] := s[i-1] + a[i];
		end;
		readln;

		found := false;
		l0 := 0;
		r0 := 0;

		for l := 1 to n-2 do
			if not found then
				for r := l+1 to n-1 do
					if not found then begin
						s1 := s[l] mod 3;
						s2 := (s[r] - s[l]) mod 3;
						s3 := (s[n] - s[r]) mod 3;
						found := (s1 <> s2) and (s2 <> s3) and (s3 <> s1);
						found := found or (s1 = s2) and (s2 = s3);
						if found then begin
							l0 := l;
							r0 := r;
						end;
					end;

		writeln(l0, ' ', r0);

	end;
end.

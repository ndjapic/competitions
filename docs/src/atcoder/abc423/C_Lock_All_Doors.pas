program C_Lock_All_Doors;
uses
	math;
const
	nn = 200 * 1000 + 1;
var
	n, i, j, r, ans1, ans2: int32;
	l, l1, l2: array [0 .. nn] of int8;

begin
	readln(n, r);

	for i := 0 to n+1 do begin
		if (1 <= i) and (i <= n) then
			read(l[i])
		else
			l[i] := 1;
		l1[i] := l[i];
		l2[i] := l[i];
	end;
	readln;

	i := 0;
	j := n;
	while (i < r) and (l[i+1] = 1) do inc(i);
	while (j > r) and (l[j] = 1) do dec(j);

	ans1 := 0;
	ans2 := 0;


	while r > i+1 do begin
		if l1[r] = 1 then begin
			l1[r] := 0;
			inc(ans1);
		end;
		dec(r);
	end;

	if (r = i+1) and (l1[r] = 0) then begin
		l1[r] := 1;
		inc(ans1);
	end;

	while r < j do begin
		if l1[r+1] = 1 then begin
			l1[r+1] := 0;
			inc(ans1);
		end;
		inc(r);
		if l1[r] = 0 then begin
			l1[r] := 1;
			inc(ans1);
		end;
	end;


	while r < j-1 do begin
		if l2[r+1] = 1 then begin
			l2[r+1] := 0;
			inc(ans2);
		end;
		inc(r);
	end;

	if (r = j-1) and (l2[r+1] = 0) then begin
		l2[r+1] := 1;
		inc(ans2);
	end;

	while r > i do begin
		if l2[r] = 1 then begin
			l2[r] := 0;
			inc(ans2);
		end;
		dec(r);
		if l2[r+1] = 0 then begin
			l2[r+1] := 1;
			inc(ans2);
		end;
	end;


	writeln(min(ans1, ans2));
end.

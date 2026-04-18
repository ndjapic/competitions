# Задатак: D_Ulam_Warburton_Automaton.pas

```pascal
program D_Ulam_Warburton_Automaton;
{$MODE DELPHI}
const
	hh = 300 * 1000;
var
	h, w, i, j, k, k2, ans: int32;
	s: array [1 .. hh] of string;
	t: array [1 .. hh] of record
		i, j: int32;
	end;
	t2: array [1 .. 4*hh] of record
		i, j: int32;
	end;

procedure enqueue(i, j: int32);
var
	c: int8;
begin
	if s[i][j] = '.' then begin

		c := 0;
		if (i > 1) and (s[i-1][j] = '#') then inc(c);
		if (i < h) and (s[i+1][j] = '#') then inc(c);
		if (j > 1) and (s[i][j-1] = '#') then inc(c);
		if (j < w) and (s[i][j+1] = '#') then inc(c);

		if c = 1 then begin
			inc(k2);
			t2[k2].i := i;
			t2[k2].j := j;
		end;

	end;
end;

begin
	readln(h, w);

	k := 0;
	for i := 1 to h do begin
		readln(s[i]);
		for j := 1 to w do
			if s[i][j] = '#' then begin
				inc(k);
				t[k].i := i;
				t[k].j := j;
			end;
	end;

	k2 := 0;
	ans := 0;
	while k > 0 do begin
		inc(ans, k);

		while k > 0 do begin

			i := t[k].i;
			j := t[k].j;
			dec(k);

			if i > 1 then enqueue(i-1, j);
			if i < h then enqueue(i+1, j);
			if j > 1 then enqueue(i, j-1);
			if j < w then enqueue(i, j+1);

		end;

		while k2 > 0 do begin
			i := t2[k2].i;
			j := t2[k2].j;
			dec(k2);
			if s[i][j] = '.' then begin
				s[i][j] := '#';
				inc(k);
				t[k].i := i;
				t[k].j := j;
			end;
		end;

	end;

	writeln(ans);
end.

```

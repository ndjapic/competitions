program D_1709;
const
	nn = 40;
var
	ntc, tci, n, i, j, k, x, ta, tb: int8;
	a, b: array [1 .. nn] of int8;
	op: array [1 .. 1709] of record
		j, i: int8;
	end;
	pos: array [1 .. 2*nn] of record
		j, i: int8;
	end;

procedure action(j, i: int8);
var
	x: int8;
begin
	inc(k);
	op[k].j := j;
	op[k].i := i;

	case j of

		1: begin
			x := a[i];
			a[i] := a[i+1];
			a[i+1] := x;
			pos[a[i]].i := i;
			pos[a[i+1]].i := i+1;
		end;

		2: begin
			x := b[i];
			b[i] := b[i+1];
			b[i+1] := x;
			pos[b[i]].i := i;
			pos[b[i+1]].i := i+1;
		end;

		3: begin
			x := a[i];
			a[i] := b[i];
			b[i] := x;
			pos[a[i]].j := 1;
			pos[b[i]].j := 2;
		end;

	end;
end;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);
		k := 0;

		for i := 1 to n do begin
			read(x);
			a[i] := x;
			pos[x].j := 1;
			pos[x].i := i;
		end;
		readln;

		for i := 1 to n do begin
			read(x);
			b[i] := x;
			pos[x].j := 2;
			pos[x].i := i;
		end;
		readln;

		ta := 1;
		tb := 1;

		for x := 1 to 2*n do begin

			i := pos[x].i;
			j := pos[x].j;

			if i >= ta then begin

				while i > ta do begin
					dec(i);
					action(j, i);
				end;
				if j = 2 then action(3, i);
				inc(ta);

			end else if (ta <= n) and (i - tb > ta - i) then begin

				while i < ta do begin
					action(j, i);
					inc(i);
				end;
				action(3, i);
				inc(ta);

			end else begin

				while i > tb do begin
					dec(i);
					action(2, i);
				end;
				inc(tb);

			end;

		end;

		writeln(k);
		for i := 1 to k do writeln(op[i].j, ' ', op[i].i);

	end;
end.

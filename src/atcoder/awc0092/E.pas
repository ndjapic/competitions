program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #segtree
const
	NN = 50 * 1000;
	TT = 128 * 1024;
	CC = 50;
var
	n, q, i, j, l, r: int32;
	c, h, tp: int8;
	a: array [1 .. NN] of int32;
	st, qt: array [1 .. TT, 0 .. CC] of record
		m, h: int32;
	end;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure build(v, vl, vr: int32);
var
	vm, g: int32;
	h: int8;
begin
	if vl = vr then begin

		for h := 0 to a[vl] - 1 do begin
			st[v, h].m := 0;
			st[v, h].h := h;
		end;

		for h := a[vl] to c do begin
			st[v, h].m := 1;
			st[v, h].h := h - a[vl];
		end;

	end else begin

		vm := (vl + vr) div 2;
		build(2*v, vl, vm);
		build(2*v+1, vm + 1, vr);

		for h := 0 to c do begin
			st[v, h].m := st[2*v, h].m;
			g := st[2*v, h].h;
			if g >= 0 then begin
				inc(st[v, h].m, st[2*v+1, g].m);
				st[v, h].h := st[2*v+1, g].h;
			end else
				st[v, h].h := g;
		end;

	end;
end;

procedure query(v, vl, vr, l, r: int32);
var
	vm, g: int32;
	h: int8;
begin
	if (r < vl) or (vr < l) then begin

		for h := 0 to c do begin
			qt[v, h].m := 0;
			qt[v, h].h := h;
		end;

	end else if (l <= vl) and (vr <= r) then begin

		for h := 0 to c do begin
			qt[v, h].m := st[v, h].m;
			qt[v, h].h := st[v, h].h;
		end;

	end else begin

		vm := (vl + vr) div 2;
		query(2*v, vl, vm, l, r);
		query(2*v+1, vm + 1, vr, l, r);

		for h := 0 to c do begin
			qt[v, h].m := qt[2*v, h].m;
			g := qt[2*v, h].h;
			if g >= 0 then begin
				inc(qt[v, h].m, qt[2*v+1, g].m);
				qt[v, h].h := qt[2*v+1, g].h;
			end else
				qt[v, h].h := g;
		end;

	end;
end;

procedure update(v, vl, vr, i, x: int32);
var
	vm, g: int32;
	h: int8;
begin
	if (i < vl) or (vr < i) then begin
	end else if (i <= vl) and (vr <= i) then begin

		for h := 0 to x - 1 do begin
			st[v, h].m := 0;
			st[v, h].h := h;
		end;

		for h := x to c do begin
			st[v, h].m := 1;
			st[v, h].h := h - x;
		end;

	end else begin

		vm := (vl + vr) div 2;
		update(2*v, vl, vm, i, x);
		update(2*v+1, vm + 1, vr, i, x);

		for h := 0 to c do begin
			st[v, h].m := st[2*v, h].m;
			g := st[2*v, h].h;
			if g >= 0 then begin
				inc(st[v, h].m, st[2*v+1, g].m);
				st[v, h].h := st[2*v+1, g].h;
			end else
				st[v, h].h := g;
		end;

	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, c, q);

	for i := 1 to n do read(a[i]);
	readln;
	build(1, 1, n);

	for j := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				readln(i, h);
				a[i] := h;
				update(1, 1, n, i, h);
			end;

			2: begin
				readln(l, r, h);
				query(1, 1, n, l, r);
				writeln(qt[1, h].m);
			end;

		end;
	end;
end.

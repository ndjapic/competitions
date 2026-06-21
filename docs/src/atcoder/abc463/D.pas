program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #heap #bisect
const
	NN = 200 * 1000;
type
	TPrioQueue<T> = class
	public
		items: array [0 .. NN] of T;
		count: int32;
		constructor Create;
		function Compare(lhs, rhs: T): int32;
		procedure setItem(v: int32; x: T);
		procedure swim(v: int32; x: T);
		procedure push(x: T);
		function prioChild(u: int32): int32;
		procedure sink(u: int32; x: T);
		procedure pop;
	end;
var
	n, k, i, bl, br, bm, x, noc: int32;
	l, r: array [1 .. NN] of int32;
	pq: TPrioQueue<Int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

constructor TPrioQueue<T>.Create;
begin
	count := 0;
end;

function TPrioQueue<T>.Compare(lhs, rhs: T): int32;
begin
	result := r[lhs] - r[rhs];
end;

procedure TPrioQueue<T>.setItem(v: int32; x: T);
begin
	items[v] := x;
end;

procedure TPrioQueue<T>.swim(v: int32; x: T);
var
	u: int32;
begin
	u := (v-1) div 2;
	while (v > 0) and (Compare(x, items[u]) < 0) do begin
		setItem(v, items[u]);
		v := u;
		u := (v-1) div 2;
	end;
	setItem(v, x);
end;

procedure TPrioQueue<T>.push(x: T);
begin
	inc(count);
	swim(count - 1, x);
end;

function TPrioQueue<T>.prioChild(u: int32): int32;
var
	v: int32;
begin
	v := u * 2 + 1;
	if (v+1 < count) and (Compare(items[v+1], items[v]) < 0) then inc(v);
	result := v;
end;

procedure TPrioQueue<T>.sink(u: int32; x: T);
var
	v: int32;
begin
	v := prioChild(u);
	while (v < count) and (Compare(items[v], x) < 0) do begin
		setItem(u, items[v]);
		u := v;
		v := prioChild(u);
	end;
	setItem(u, x);
end;

procedure TPrioQueue<T>.pop;
begin
	dec(count);
	if count > 0 then sink(0, items[count]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	for i := 1 to n do readln(l[i], r[i]);

	bl := 0;
	br := 1 shl 30;
	while br - bl > 1 do begin
		bm := (bl + br) div 2;
		pq := TPrioQueue<Int32>.Create;
		try

			for i := 1 to n do pq.push(i);

			noc := 0;
			x := 0;
			while (pq.count > 0) and (noc < k) do begin

				while (pq.count > 0) and (l[pq.items[0]] < x) do pq.pop;

				if pq.count > 0 then begin
					i := pq.items[0];

					if l[i] >= x then begin
						inc(noc);
						x := r[i] + bm;
					end;
				end;

			end;

			if noc >= k then
				bl := bm
			else
				br := bm;

		finally
			pq.Free;
		end;
	end;

	if bl = 0 then bl := -1;
	writeln(bl);
end.

{$mode delphi}
program SparseTableIndexRMQ;
uses
	SysUtils,
	Generics.Collections, 
	Generics.Defaults;
type
	{ ТГенеричка Ретка Табела која враћа индекс минималног елемента }
	TSparseTable<T> = class
	private
		FTable: array of array of int32; // Чува индексе
		FLg: array of int32;
		FData: array of T; // Интерно чувамо копију низа за поређење
		FComparer: IComparer<T>;
		procedure PrecomputeLogarithms(Size: int32);
	public
		// Конструктор прихвата динамички низ
		constructor Create(const Arr: array of T); overload;
		// Алтернативни конструктор који прихвата TList<T> ради комоције
		constructor Create(AList: TList<T>); overload;
		// Враћа ИНДЕКС минималног елемента у распону [L, R]
		function QueryIndex(L, R: int32): int32;
	end;

{ TSparseTable }

procedure TSparseTable<T>.PrecomputeLogarithms(Size: int32);
var
	I: int32;
begin
	SetLength(FLg, Size + 1);
	FLg[0] := 0;
	FLg[1] := 0;
	for I := 2 to Size do
		FLg[I] := FLg[I div 2] + 1;
end;

constructor TSparseTable<T>.Create(const Arr: array of T);
var
	N, MaxLog, I, J: int32;
	Idx1, Idx2: int32;
begin
	N := Length(Arr);
	if N = 0 then Exit;

	FComparer := TComparer<T>.Default;
	
	// Копирамо податке у интерни низ како бисмо могли да их поредимо током упита
	SetLength(FData, N);
	for I := 0 to N - 1 do
		FData[I] := Arr[I];

	PrecomputeLogarithms(N);
	MaxLog := FLg[N] + 1;
	SetLength(FTable, N, MaxLog);

	// Базни случај: индекс распона дужине 1 је сам тај индекс
	for I := 0 to N - 1 do
		FTable[I, 0] := I;

	// Попуњавање табеле индексима
	for J := 1 to MaxLog - 1 do
	begin
		I := 0;
		while (I + (1 shl J)) <= N do
		begin
			Idx1 := FTable[I, J - 1];
			Idx2 := FTable[I + (1 shl (J - 1)), J - 1];

			// Поредимо вредности на тим индексима, памтимо индекс мање вредности
			if FComparer.Compare(FData[Idx1], FData[Idx2]) <= 0 then
				FTable[I, J] := Idx1
			else
				FTable[I, J] := Idx2;

			Inc(I);
		end;
	end;
end;

constructor TSparseTable<T>.Create(AList: TList<T>);
begin
	// Претварамо TList у динамички низ и позивамо главни конструктор
	Create(AList.ToArray);
end;

function TSparseTable<T>.QueryIndex(L, R: int32): int32;
var
	Len, K: int32;
	Idx1, Idx2: int32;
begin
	Len := R - L + 1;
	K := FLg[Len];

	Idx1 := FTable[L, K];
	Idx2 := FTable[R - (1 shl K) + 1, K];

	// Враћамо онај индекс чија је вредност мања
	if FComparer.Compare(FData[Idx1], FData[Idx2]) <= 0 then
		Result := Idx1
	else
		Result := Idx2;
end;

// --- Демонстрација рада ---
var
	// Тест са целим бројевима помоћу TList-а (комотнија варијанта)
	IntList: TList<int32>;
	IntRMQ: TSparseTable<int32>;
	MinIdx: int32;

	// Тест са стринговима помоћу обичног низа
	StrArray: array of string;
	StrRMQ: TSparseTable<string>;
begin
	{ 1. ТЕСТ СА ЦЕЛИМ БРОЈЕВИМА (Преко TList-а) }
	IntList := TList<int32>.Create;
	try
		// Попуњавамо листу динамички (комотно)
		IntList.AddRange([7, 2, 3, 0, 5, 10, 3, 12, 18]);

		IntRMQ := TSparseTable<int32>.Create(IntList);
		try
			Writeln('--- Test sa celim brojevima ---');

			MinIdx := IntRMQ.QueryIndex(0, 4);
			Writeln(Format('Raspon [0,4]: Minimalni indeks je %d (Vrednost: %d)', [MinIdx, IntList[MinIdx]]));

			MinIdx := IntRMQ.QueryIndex(4, 7);
			Writeln(Format('Raspon [4,7]: Minimalni indeks je %d (Vrednost: %d)', [MinIdx, IntList[MinIdx]]));
		finally
			IntRMQ.Free;
		end;
	finally
		IntList.Free;
	end;

	Writeln;

	{ 2. ТЕСТ СА СТРИНГОВИМА (Преко низа, лексикографски минимум) }
	StrArray := ['Zebra', 'Vuk', 'Galeb', 'Ajkula', 'Medved'];
	StrRMQ := TSparseTable<string>.Create(StrArray);
	try
		Writeln('--- Test sa stringovima ---');
		MinIdx := StrRMQ.QueryIndex(0, 2); // 'Zebra', 'Vuk', 'Galeb' -> Минимум је 'Galeb'
		Writeln(Format('Raspon [0,2]: Minimalni indeks je %d (Vrednost: %s)', [MinIdx, StrArray[MinIdx]]));
	finally
		StrRMQ.Free;
	end;

	Write(#10 + 'Pritisnite Enter за крај...');
	Readln;
end.

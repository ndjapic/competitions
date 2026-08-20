program SlidingWindowWithCustomDeque;
{$MODE DELPHI}
type
	// Дефиниција сопствене генеричке класе за дек
	TDeque<T> = class
	private
		FData: array of T;
		FHead: Int32;
		FTail: Int32;
	public
		// Конструктор прихвата максимални капацитет ради брзине
		constructor Create(ACapacity: Int32);

		procedure PushBack(const Item: T);
		procedure PopBack;
		procedure PopFront;

		function GetFront: T;
		function GetBack: T;
		function Count: Int32;
	end;

{ TDeque<T> Имплементација метода }

constructor TDeque<T>.Create(ACapacity: Int32);
begin
	SetLength(FData, ACapacity);
	FHead := 0;
	FTail := -1;
end;

procedure TDeque<T>.PushBack(const Item: T);
begin
	Inc(FTail);
	FData[FTail] := Item;
end;

procedure TDeque<T>.PopBack;
begin
	Dec(FTail);
end;

procedure TDeque<T>.PopFront;
begin
	Inc(FHead);
end;

function TDeque<T>.GetFront: T;
begin
	Result := FData[FHead];
end;

function TDeque<T>.GetBack: T;
begin
	Result := FData[FTail];
end;

function TDeque<T>.Count: Int32;
begin
	Result := FTail - FHead + 1;
end;

{ Главни програм: Решавање проблема минимума поднизова }

const
	N = 8;
	D = 3;

var
	h: array [0 .. N-1] of Int32 = (4, 3, 2, 1, 5, 7, 6, 8);
	dq: TDeque<Int32>; // Дек који чува индексе типа Int32
	i: Int32;

begin
	// Креирамо дек са капацитетом N јер чувамо индексе низа
	dq := TDeque<Int32>.Create(N);
	try
		Writeln('Минимуми поднизова дужине ', D, ':');

		for i := 0 to N - 1 do begin
			// 1. Избацивање индекса који су испали из прозора са леве стране
			if (dq.Count > 0) and (dq.GetFront <= i - D) then
				dq.PopFront;

			// 2. Избацивање са десне стране свих чија је вредност у низу h већа од тренутне h[i]
			while (dq.Count > 0) and (h[dq.GetBack] >= h[i]) do
				dq.PopBack;

			// 3. Додавање тренутног индекса на крај дека
			dq.PushBack(i);

			// 4. Штампање резултата када прозор достигне пуну величину D
			if i >= D - 1 then
				Write(h[dq.GetFront], ' ');
		end;
		Writeln;

	finally
		dq.Free; // Обавезно ослобађање меморије објекта
	end;
end.

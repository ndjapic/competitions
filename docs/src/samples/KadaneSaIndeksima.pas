program KadaneSaIndeksima;

{$MODE DELPHI}

uses
	Math;

const
	NIZ_VELICINA = 8;

type
	TStatickiNiz = array[1..NIZ_VELICINA] of Integer;

// Procedura vraća sumu, ali i indeks početka i kraja preko var parametara
procedure NadjiMaxPodniz(const Arr: TStatickiNiz; var MaxSuma, StartIndeks, KrajIndeks: Integer);
var
	MaxEndingHere: Integer;
	TrenutniStart: Integer;
	I: Integer;
begin
	// Inicijalizacija prvim elementom niza
	MaxSuma := Arr;
	MaxEndingHere := Arr;
	StartIndeks := 1;
	KrajIndeks := 1;
	TrenutniStart := 1;

	for I := 2 to NIZ_VELICINA do
	begin
		// Ako je element veći od dosadašnjeg zbira sa tim elementom, počinje nov podniz
		if Arr[I] > MaxEndingHere + Arr[I] then
		begin
			MaxEndingHere := Arr[I];
			TrenutniStart := I; // Pamti se novi potencijalni početak
		end
		else
		begin
			MaxEndingHere := MaxEndingHere + Arr[I];
		end;

		// Ako smo pronašli novu, veću ukupnu sumu
		if MaxEndingHere > MaxSuma then
		begin
			MaxSuma := MaxEndingHere;
			StartIndeks := TrenutniStart; // Beleži se konačni početak
			KrajIndeks := I;            // Beleži se kraj podniza
		end;
	end;
end;

var
	Numbers: TStatickiNiz;
	Suma, Pocetak, Kraj, I: Integer;
begin
	Numbers := -2;
	Numbers := -3;
	Numbers := 4;
	Numbers := -1;
	Numbers := -2;
	Numbers := 1;
	Numbers := 5;
	Numbers := -3;

	NadjiMaxPodniz(Numbers, Suma, Pocetak, Kraj);
	
	Writeln('Maksimalna suma podniza je: ', Suma);
	Writeln('Podniz pocinje na indeksu ', Pocetak, ' i zavrsava se na indeksu ', Kraj);
	
	Write('Elementi podniza su: ');
	for I := Pocetak to Kraj do
		Write(Numbers[I], ' ');
	Writeln;

	Readln;
end.

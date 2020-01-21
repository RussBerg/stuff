; Copyright (c) 1998-2000 A.P. Hitchcock  All rights reserved
;+
;NAME:
;	ALS_SPECTRA_EVENTCB
;
;LAST CHANGED: ----------------------------------- 31-dec-99
;
;PURPOSE:
;	This IDL Event Callback procedure processes the events generated
;	by the buttons in the >READ>ALS-STXM spectral input command of AXIS
;
;CATEGORY:
;	AXIS spectra analysis
;	IDL Event Callback Procedure for als_spectra_eventcb
;
;ROUTINES:
;	ALS_Spectra_Update - indicate current values of read-in parameters
;	BUT_I_all		BUT_I_none		BUT_I_dark		BUT_I_OSA		BUT_I_both
;	BUT_Io_all		BUT_Io_none		BUT_Io_dark		BUT_Io_OSA		BUT_Io_both
;	TXT_I_check		TXT_I_col		BUT_I_Browse
;	TXT_Io_check	TXT_Io_col		BUT_Io_Browse
;	Scan_Par_Update
;	BUT_Load_Sample
;	BUT_Load_ratio
;	BUT_Load_Absorption
;	BUT_Load_Data
;	BUT_Cancel
;	ALS_SPEC_EVENTCB - stub routine to load all the rest
;
;INPUTS: none
;
;KEYWORDS: none
;
;OUTPUTS: none
;
;COMMON BLOCKS:
;	@AXIS_COM	standard set of common blocks
;
;RESTRICTIONS:
; Whenever re-generate ALS_Spectra, add the following to pro BSE_ALS_SPECTRA,:
; to ADD an explicit Font command to handle different
; choices of Font Size at Windows level
;  @axis_com
;  WIDGET_CONTROL, Default_Font = BufLblFont
;
; NB -- Remove the All_Events keyword from the creation
;       of the TXT_I_check and TXT_Io_check text widgets in ALS_Spectra,pro
;		to trigger events only on newlines.
; END OF STUFF to add to pro BSE_ALS_SPECTRA, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
;
;MODIFICATIONS
; First generated on:	01/01/99 16:14.31
; (03-jan-99 aph) add ytitle (sample.yl) to returned Data
; (08-jan-99 aph) change display of parameters to table widget to get round overlap crash
; (11-jan-99 aph) error recovery if request reference with no file defined
; (28-jan-99 aph) DefPath for path and get_path
; (25-jun-99 aph) remove ambiguity of Scan_par_Update (function) & ScanPar_Update (paramater)
; (04-jul-99 aph) correct error in last procedure !
; (10-sep-99 aph) correct error in interpolation
; (30-dec-99 aph) add AXIS documentation
;-

pro ALS_Spectra_Update, wWidget
; loads current default values into ALS_SPECTRA widget

@axis_com

id = widget_info(wWidget, FIND_BY_UNAME = 'TXT_I_File')
WIDGET_CONTROL, id, Set_Value = S_file

id = widget_info(wWidget, FIND_BY_UNAME = 'TXT_Io_File')
WIDGET_CONTROL, id, Set_Value = Io_file

if I_Process EQ '' then I_Process = 'none'
id = widget_info(wWidget, FIND_BY_UNAME = 'BUT_I_'+strcompress(I_Process))
WIDGET_CONTROL, id, Set_Button = 1

if Io_Process EQ '' then Io_Process = 'none'
id = widget_info(wWidget, FIND_BY_UNAME = 'BUT_Io_'+strcompress(Io_Process))
WIDGET_CONTROL, id, Set_Button = 1

if ALS_readin EQ '' then ALS_readin = 'data'
id = widget_info(wWidget, FIND_BY_UNAME = 'BUT_Load_'+strcompress(ALS_readin))
WIDGET_CONTROL, id, Set_Button = 1

if n_elements(I_col) EQ 0 then I_col = 2
id = widget_info(wWidget, FIND_BY_UNAME = 'TXT_I_col')
WIDGET_CONTROL, id, Set_Value = strcompress(string(I_col))

if n_elements(Io_col) EQ 0 then Io_col = 2
id = widget_info(wWidget, FIND_BY_UNAME = 'TXT_Io_col')
WIDGET_CONTROL, id, Set_Value = strcompress(string(Io_col))

if scanpar_update EQ '' then scanpar_update = 'on'
if scanpar_update EQ 'on' then begin
	id = widget_info(wWidget, FIND_BY_UNAME = 'BUT_Browse_Update')
	WIDGET_CONTROL, id, Set_Button = 1
endif else WIDGET_CONTROL, id, Set_Button = 0

END

; ****************set value for Processing controls **************

;-----------------------------------------------------------------
pro BUT_I_all, Event
@axis_com
	I_process = 'all'
end
;-----------------------------------------------------------------
pro BUT_I_none, Event
@axis_com
	I_process = 'none'
end
;-----------------------------------------------------------------
pro BUT_I_dark, Event
@axis_com
	I_process = 'dark'
end
;-----------------------------------------------------------------
pro BUT_I_OSA, Event
@axis_com
	I_process = 'OSA'
end
;-----------------------------------------------------------------
pro BUT_I_both, Event
@axis_com
	I_process = 'both'
end

;-----------------------------------------------------------------
pro BUT_Io_all, Event
@axis_com
	Io_process = 'all'
end
;-----------------------------------------------------------------
pro BUT_Io_none, Event
@axis_com
	Io_process = 'none'
end
;-----------------------------------------------------------------
pro BUT_Io_dark, Event
@axis_com
	Io_process = 'dark'
end
;-----------------------------------------------------------------
pro BUT_Io_OSA, Event
@axis_com
	Io_process = 'OSA'
end
;-----------------------------------------------------------------
pro BUT_Io_both, Event
@axis_com
	Io_process = 'both'
end

; ************************** define filenames **************
;-----------------------------------------------------------------
pro TXT_I_check, Event
; update sample file
@axis_com
   	id = widget_info(Event.top, FIND_BY_UNAME = 'TXT_I_File')
	WIDGET_CONTROL, id, Get_Value = test
	S_file = string(test(0)) &  print, 'Sample file set to ', S_file
end


;-----------------------------------------------------------------
pro BUT_I_Browse, Event
; --------------------- last changed:  01-Jan-99
; used dialog to select I file
@axis_com
; get sample file
   	id = widget_info(Event.top, FIND_BY_UNAME = 'TXT_I_File')
	WIDGET_CONTROL, id, Get_Value = test
	S_file = string(test(0))
;	print, 'Current ini file is ', S_file
; check if it exists
	test = FINDFILE(S_file,count=cnt)
;	print, cnt,' files found' & print, test
	if cnt EQ 1 then begin
		S_file = string(test(0))
		path = strmid(S_file,0,strlen(S_file)-12)
	endif else begin
		S_file = ''
		path = DefPath	; default to last data path
	endelse
; pop-up file selector, with S_file if it exists
	S_file = dialog_pickfile(file=S_file, filter='*.xas',get_Path=DefPath, path=DefPath, title='select sample file')
; update text widget with S_file name unless no value
 	IF S_file NE '' then WIDGET_CONTROL, id, Set_Value = S_file
; read file and update scan parameters if requested
	IF scanpar_update EQ 'on' then scan_par_update, Event
end

;-----------------------------------------------------------------
pro TXT_Io_check, Event
@axis_com
id = widget_info(Event.top, FIND_BY_UNAME = 'TXT_Io_File')
WIDGET_CONTROL,id, Get_Value = test
Io_File =  string(test(0))  &  print, 'Reference file set to ', Io_file
end

;-----------------------------------------------------------------
pro BUT_Io_Browse, Event
; --------------------- last changed:  01-Jan-99
; used dialog to select I file
@axis_com
; get sample file
   	id = widget_info(Event.top, FIND_BY_UNAME = 'TXT_Io_File')
	WIDGET_CONTROL, id, Get_Value = test
	Io_file = string(test(0))
;	print, 'Current ini file is ', Io_file
; check if it exists
	test = FINDFILE(Io_file,count=cnt)
;	print, cnt,' files found' & print, test
	if cnt EQ 1 then begin
		Io_file = string(test(0))
		path = strmid(Io_file,0,strlen(Io_file)-12)
	endif else begin
		Io_file = ''
		path = DefPath	; default to last data path
	endelse
; pop-up file selector, with Io_file if it exists
	Io_file = dialog_pickfile(file=Io_file, filter='*.xas', get_Path=DefPath, path=DefPath, title='select reference file')
; update text widget with Io_file name unless no value
 	IF Io_file NE '' then WIDGET_CONTROL, id, Set_Value = Io_file
; read file and update scan parameters if requested
	IF scanpar_update EQ 'on' then scan_par_update, Event, Ref=1
end


; ***************** Select data column **************************
;-----------------------------------------------------------------
pro TXT_I_col, Event
@axis_com
   	id = widget_info(Event.top, FIND_BY_UNAME = 'TXT_I_col')
	WIDGET_CONTROL, id, Get_Value = test
	I_col = fix(test(0))
;	print, 'Sample data column changed to # ', I_col
end

;-----------------------------------------------------------------
pro TXT_Io_col, Event
@axis_com
   	id = widget_info(Event.top, FIND_BY_UNAME = 'TXT_Io_col')
	WIDGET_CONTROL, id, Get_Value = test
	Io_col = fix(test(0))
;	print, 'Reference data column changed to # ', Io_col
end

; **************** Update scan paramaters if requested **************
pro BUT_Browse_Update, Event
@axis_com
	IF scanpar_update EQ 'on' then scanpar_update = 'off' else scanpar_update='on'
;	print, 'Update scan parameters is ', scanpar_update
end

; **************** Update scan paramaters if requested **************
pro Scan_Par_Update, Event, Ref=Ref
; -------------------- last changed: 08-Jan-99
; updates ALS_SPECTRA widget with scan values of sample or reference file
; set Keyword REF to switch fro sample to reference boxes

; ---------------REVISION HISTORY -------------------------
; (aph 02-Jan-99) first written
; (aph 08-Jan-99 change to table display

@axis_com
IF keyword_set(ref) then BEGIN
;	print, 'Reference file'
	sr = 'Io'
	tmp = ALS_Specin(Io_file, col = Io_col, REF=1)
ENDIF ELSE BEGIN
;	print, 'Sample file'
	sr = 'I'
	tmp = ALS_Specin(S_file, col = I_col)
ENDELSE
;print, 'ALS spectrum: #-cols',fix(tmp.nycol), ' #-dark: ', fix(tmp.nd),'.  average dark: ',tmp.drk
;print, '#sect. ',tmp.nsect, ' Emin ', min(tmp.x), ' Emax ', max(tmp.x)
;print, 'dwell (ms)', tmp.dwell

; update table
id = widget_info(Event.top, FIND_BY_UNAME = 'TBL_'+sr)
tbl_val=strarr(2, 8)
tbl_val(0,0) = strcompress(string(format='(F5.1)', tmp.dwell))
tbl_val(0,1) = strcompress(string(format='(F7.2)',min(tmp.x)))
tbl_val(0,2) = strcompress(string(format='(F7.2)',max(tmp.x)))
tbl_val(0,3) = strcompress(string(format='(I2)',fix(tmp.nsect)))
tbl_val(0,4) = strcompress(string(format='(I2)',fix(tmp.nycol)))
tbl_val(0,5) = strcompress(string(format='(I2)',fix(tmp.nd)))
tbl_val(0,6) = strcompress(string(format='(F7.2)',1000*tmp.drk))
WIDGET_CONTROL, id, Set_Value = tbl_val, USE_Table_Select = [0,0,0,6]


end

; ********************* Set type of read-in *********************
;-----------------------------------------------------------------
pro BUT_Load_Sample, Event
@axis_com
	als_readin = 'data'
end
;-----------------------------------------------------------------
pro BUT_Load_ratio, Event
@axis_com
	als_readin = 'ratio'
end
;-----------------------------------------------------------------
pro BUT_Load_Absorption, Event
@axis_com
	als_readin = 'absorption'
end

; *********************** DO IT !! ******************************
;-----------------------------------------------------------------
pro BUT_Load_Data, Event
; ----------------------- last changed: 02-Jan-99
; reads in ALS spectral data converted according to switches on widget
; correction procedure for I, Io determined by I_process, Io_process common variables

@axis_com

; get sample file
   	id = widget_info(Event.top, FIND_BY_UNAME = 'TXT_I_File')
	WIDGET_CONTROL, id, Get_Value = test
	S_file = string(test(0))
; check if it exists
	test = FINDFILE(S_file,count=cnt)
	IF cnt EQ 1 then BEGIN
		WIDGET_CONTROL, /Hourglass
;		print, 'Col = ', I_col
		sample = ALS_Specin(S_file, COL=I_col)
	ENDIF ELSE BEGIN
		S_file = 'No file'
		WIDGET_CONTROL, id, Set_Value = S_file
	ENDELSE


IF ALS_Readin NE 'data' THEN BEGIN
; get reference file
   	id = widget_info(Event.top, FIND_BY_UNAME = 'TXT_Io_File')
	WIDGET_CONTROL, id, Get_Value = test
	Io_file = string(test(0))
	test = FINDFILE(Io_file,count=cnt)
	IF cnt EQ 1 then BEGIN
		IF S_file EQ 'No file' then return  ; protect against fools
		WIDGET_CONTROL, /Hourglass
		reference = ALS_Specin(Io_file, COL=Io_col, ref = 1)
;		print, 'Reference: #-dark: ', fix(sample.nd),'.  average dark: ',sample.drk

; INTERPOLATE reference and sample to common E-scale
		xmin = fltarr(2) & xmax = fltarr(2)  ; identify common range
		xmin(0) = min(sample.x) & xmin(1) = min(reference.x)
		xmax(0) = max(sample.x) & xmax(1) = max(reference.x)
		xmin = max(xmin) & xmax = min(xmax)
		xval1 = where(sample.x GE xmin AND sample.x LE xmax, nt1)
		xval2 = where(reference.x GE xmin AND reference.x LE xmax, nt2)
		if nt1 NE 0 AND nt2 NE 0 then BEGIN
			x1 = sample.x(where(sample.x GE xmin AND sample.x LE xmax))   ; trim arrays to common range
			d1 = sample.d(where(sample.x GE xmin AND sample.x LE xmax))   ; NB must use intermediate arrays!!
			x2 = reference.x(where(reference.x GE xmin AND reference.x LE xmax))
			d2 = reference.d(where(reference.x GE xmin AND reference.x LE xmax))
			if nt1 GE nt2 THEN BEGIN
				y1 = d1  & x = x1
				y2 = Interpol(d2, x2, x1)        ; mesh to densest data set
			endif else BEGIN
				y2 = d2 & x = x2
			    y1 = Interpol(d1, x1, x2)
			endelse
			t = {t:'1d', x: x, d: y1, dl: sample.dl, xl:sample.xl, yl:' ', dn: y1}
			sample = t
			t = {t:'1d', x: x, d: y2, dl: reference.dl, xl:reference.xl, yl:' ', dn: y2}
			reference = t
		endif
	ENDIF ELSE BEGIN
		Io_file = 'No file'
		WIDGET_CONTROL, id, Set_Value = Io_file
	ENDELSE
ENDIF

CASE ALS_readin OF

'data' : BEGIN
	IF S_file EQ 'No file' then return
	tmp={t:'1d', d:sample.d, x:sample.x, xl:'Photon Energy (eV)', yl: sample.yl, $
	     dn:sample.dn, dl:sample.dl}
	HANDLE_VALUE, Data(CurBuf), sample, /SET
   	WIDGET_CONTROL, Event.Top, /DESTROY
END

'ratio' : BEGIN
	IF S_file EQ 'No file' OR Io_file EQ 'No file' then return
	sample.d = sample.d / reference.d
	sample.dl = sample.dl + '/' + reference.dl
	sample.yl = 'Ratio to reference signal'
	tmp={t:'1d', d:sample.d, x:sample.x, xl:'Photon Energy (eV)', yl: sample.yl, $
	     dn:sample.dn, dl:sample.dl}
	HANDLE_VALUE, Data(CurBuf), sample, /SET
   	WIDGET_CONTROL, Event.Top, /DESTROY
END

'absorption' : BEGIN
	IF S_file EQ 'No file' OR Io_file EQ 'No file' then return
	sample.d = alog(reference.d/sample.d)
	sample.dl = 'Abs '+ sample.dl + '; Io= ' + reference.dl
	sample.yl = 'Optical density'
	tmp={t:'1d', d:sample.d, x:sample.x, xl:'Photon Energy (eV)', yl: sample.yl, $
	     dn:sample.dn, dl:sample.dl}
	HANDLE_VALUE, Data(CurBuf), sample, /SET
   	WIDGET_CONTROL, Event.Top, /DESTROY
END

ELSE : WIDGET_CONTROL, UPrompt, Set_Value = 'ALS_spectral readin: undetermined request'
ENDCASE
end

;-----------------------------------------------------------------
pro BUT_Cancel, Event
; ----------------------------- last changed:  01-Jan-99
   	 WIDGET_CONTROL, Event.Top, /DESTROY
end
;
; Empty stub procedure used for autoloading.
;
pro als_spectra_eventcb
end


; ===============================

; Copyright (c) 1998-2000 A.P. Hitchcock  All rights reserved
;+
;NAME:
; ALS_SPECTRA
;
;LAST CHANGED: ----------------------------------- 31-dec-99
;
; PURPOSE:
;	These procedures generate a widget used to read in ALS BL 7.0 STXM
; spectral data.
;
; CATEGORY:
;	AXIS: spectra analysis
;
; CALLING SEQUENCE:
;	als_spectra [, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_ ]
;
; ROUTINES:
;	BSE_ALS_SPECTRA_event, Event - processes the events generated by user
;                            using the ALS_SPECTRA_EVENTCB routines
;	BSE_ALS_SPECTRA, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_ - generates widget structure
;	ALS_SPECTRA - stub for loading
;
; INPUTS:
;	none
;
; KEYWORDS:
;	GROUP_LEADER: ID of calling widget
;
; OUTPUTS:
;	read in of data occurs when the 'GO' button is pushed
;	code for read-in is in ALS_SPECTRA_EVENTCB callback processing
;
; COMMON BLOCKS:
;	AXIS_COM	standard set of common blocks
;
; MODIFICATION HISTORY:
; Generated on:	01/08/99 12:58.18
; (30-dec-99 aph) AXIS standard documentation
;-

pro BSE_ALS_SPECTRA_event, Event

  wWidget =  Event.top

  case Event.id of

    Widget_Info(wWidget, FIND_BY_UNAME='BSE_ALS_SPECTRA'): begin
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_Io_Browse'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_Io_Browse, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_I_Browse'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_I_Browse, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_I_all'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_I_all, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_I_none'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_I_none, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_I_dark'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_I_dark, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_I_OSA'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_I_OSA, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_I_both'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_I_both, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_Io_all'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_Io_all, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_Io_none'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_Io_none, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_Io_dark'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_Io_dark, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_Io_OSA'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_Io_OSA, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_Io_both'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_Io_both, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_Load_Data'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_Load_Data, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_Cancel'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_Cancel, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_Load_data'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_Load_Sample, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_Load_ratio'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_Load_ratio, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_Load_absorption'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_Load_Absorption, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BUT_Browse_Update'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        BUT_Browse_Update, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='TXT_I_col'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_CH' )then $
        TXT_I_col, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='TXT_Io_col'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TEXT_CH' )then $
        TXT_Io_col, Event
    end
    else:
  endcase

end

pro BSE_ALS_SPECTRA, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

; Floating or modal bases must have a Group Leader.

  if(N_Elements(wGroup) eq 0)then $
     Message,'Group leader must be specified for Modal or Floating'+ $
      ' top level bases'


  BSE_ALS_SPECTRA = Widget_Base( GROUP_LEADER=wGroup,  $
      UNAME='BSE_ALS_SPECTRA' ,SCR_XSIZE=700 ,SCR_YSIZE=406  $
      ,NOTIFY_REALIZE='ALS_Spectra_Update' ,TITLE='ALS spectral'+ $
      ' read-in' ,SPACE=3 ,XPAD=3 ,YPAD=3 ,/MODAL)


  BUT_Io_Browse = Widget_Button(BSE_ALS_SPECTRA,  $
      UNAME='BUT_Io_Browse' ,XOFFSET=450 ,YOFFSET=74 ,/ALIGN_CENTER  $
      ,VALUE='Browse')


  TXT_Io_File = Widget_Text(BSE_ALS_SPECTRA, UNAME='TXT_Io_File'  $
      ,XOFFSET=163 ,YOFFSET=77 ,SCR_XSIZE=240 ,SCR_YSIZE=24  $
      ,/EDITABLE ,/ALL_EVENTS ,/NO_NEWLINE ,XSIZE=20 ,YSIZE=1)


  BUT_I_Browse = Widget_Button(BSE_ALS_SPECTRA, UNAME='BUT_I_Browse'  $
      ,XOFFSET=450 ,YOFFSET=20 ,/ALIGN_CENTER ,VALUE='Browse')


  TXT_I_File = Widget_Text(BSE_ALS_SPECTRA, UNAME='TXT_I_File'  $
      ,XOFFSET=163 ,YOFFSET=25 ,SCR_XSIZE=240 ,SCR_YSIZE=24  $
      ,/EDITABLE ,/ALL_EVENTS ,/KBRD_FOCUS_EVENTS ,XSIZE=20 ,YSIZE=1)


  LBL_Io_File = Widget_Label(BSE_ALS_SPECTRA, UNAME='LBL_Io_File'  $
      ,XOFFSET=162 ,YOFFSET=58 ,/ALIGN_LEFT ,VALUE='Reference (Io)')


  LBL_I_File = Widget_Label(BSE_ALS_SPECTRA, UNAME='LBL_I_File'  $
      ,XOFFSET=164 ,YOFFSET=7 ,SCR_XSIZE=57 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='Sample')


  BSE_I_Process = Widget_Base(BSE_ALS_SPECTRA, UNAME='BSE_I_Process'  $
      ,FRAME=1 ,XOFFSET=6 ,YOFFSET=23 ,SCR_XSIZE=149 ,SCR_YSIZE=145  $
      ,COLUMN=1 ,/EXCLUSIVE)


  BUT_I_all = Widget_Button(BSE_I_Process, UNAME='BUT_I_all'  $
      ,/ALIGN_LEFT ,VALUE='dark + data')


  BUT_I_none = Widget_Button(BSE_I_Process, UNAME='BUT_I_none'  $
      ,/ALIGN_LEFT ,/NO_RELEASE ,VALUE='no correction')


  BUT_I_dark = Widget_Button(BSE_I_Process, UNAME='BUT_I_dark'  $
      ,/ALIGN_LEFT ,/NO_RELEASE ,VALUE='subtract dark counts')


  BUT_I_OSA = Widget_Button(BSE_I_Process, UNAME='BUT_I_OSA'  $
      ,/ALIGN_LEFT ,/NO_RELEASE ,VALUE='normalize to OSA')


  BUT_I_both = Widget_Button(BSE_I_Process, UNAME='BUT_I_both'  $
      ,/ALIGN_LEFT ,VALUE='dark, OSA')


  LBL_I_process = Widget_Label(BSE_ALS_SPECTRA, UNAME='LBL_I_process'  $
      ,XOFFSET=5 ,YOFFSET=5 ,SCR_XSIZE=126 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
      ,VALUE='Process sample file')


  LBL_Io_process = Widget_Label(BSE_ALS_SPECTRA,  $
      UNAME='LBL_Io_process' ,XOFFSET=8 ,YOFFSET=186 ,SCR_XSIZE=124  $
      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='Process reference file')


  BSE_Io_Process = Widget_Base(BSE_ALS_SPECTRA,  $
      UNAME='BSE_Io_Process' ,FRAME=1 ,XOFFSET=6 ,YOFFSET=207  $
      ,SCR_XSIZE=152 ,SCR_YSIZE=139 ,COLUMN=1 ,/EXCLUSIVE)


  BUT_Io_all = Widget_Button(BSE_Io_Process, UNAME='BUT_Io_all'  $
      ,/ALIGN_LEFT ,VALUE='dark + data')


  BUT_Io_none = Widget_Button(BSE_Io_Process, UNAME='BUT_Io_none'  $
      ,/ALIGN_LEFT ,/NO_RELEASE ,VALUE='no correction')


  BUT_Io_dark = Widget_Button(BSE_Io_Process, UNAME='BUT_Io_dark'  $
      ,/ALIGN_LEFT ,/NO_RELEASE ,VALUE='subtract dark counts')


  BUT_Io_OSA = Widget_Button(BSE_Io_Process, UNAME='BUT_Io_OSA'  $
      ,/ALIGN_LEFT ,/NO_RELEASE ,VALUE='normalize to OSA')


  BUT_Io_both = Widget_Button(BSE_Io_Process, UNAME='BUT_Io_both'  $
      ,/ALIGN_LEFT ,VALUE='dark, OSA')


  BUT_Load_Data = Widget_Button(BSE_ALS_SPECTRA,  $
      UNAME='BUT_Load_Data' ,XOFFSET=531 ,YOFFSET=86 ,SCR_XSIZE=148  $
      ,SCR_YSIZE=49 ,/ALIGN_CENTER ,VALUE='GO')


  BUT_Cancel = Widget_Button(BSE_ALS_SPECTRA, UNAME='BUT_Cancel'  $
      ,XOFFSET=599 ,YOFFSET=330 ,SCR_XSIZE=79 ,SCR_YSIZE=35  $
      ,/ALIGN_CENTER ,VALUE='Cancel')


  BSE_Abs = Widget_Base(BSE_ALS_SPECTRA, UNAME='BSE_Abs' ,FRAME=1  $
      ,XOFFSET=530 ,YOFFSET=186 ,SCR_XSIZE=150 ,SCR_YSIZE=94  $
      ,COLUMN=1 ,/EXCLUSIVE)


  BUT_Load_data = Widget_Button(BSE_Abs, UNAME='BUT_Load_data'  $
      ,/ALIGN_LEFT ,/NO_RELEASE ,VALUE='sample')


  BUT_Load_ratio = Widget_Button(BSE_Abs, UNAME='BUT_Load_ratio'  $
      ,/ALIGN_LEFT ,/NO_RELEASE ,VALUE='sample/reference')


  BUT_Load_absorption = Widget_Button(BSE_Abs,  $
      UNAME='BUT_Load_absorption' ,/ALIGN_LEFT ,/NO_RELEASE  $
      ,VALUE='Absorption: ln(Io/I)')


  LBL_ReadIn = Widget_Label(BSE_ALS_SPECTRA, UNAME='LBL_ReadIn'  $
      ,XOFFSET=530 ,YOFFSET=161 ,SCR_XSIZE=80 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='Read in:')


  WID_BASE_1 = Widget_Base(BSE_ALS_SPECTRA, UNAME='WID_BASE_1'  $
      ,XOFFSET=163 ,YOFFSET=128 ,SCR_XSIZE=296 ,SCR_YSIZE=30  $
      ,COLUMN=1 ,/NONEXCLUSIVE)


  BUT_Browse_Update = Widget_Button(WID_BASE_1,  $
      UNAME='BUT_Browse_Update' ,XOFFSET=147 ,/ALIGN_LEFT  $
      ,VALUE='Update scan parameters on Browse')


  TXT_I_col = Widget_Text(BSE_ALS_SPECTRA, UNAME='TXT_I_col'  $
      ,XOFFSET=409 ,YOFFSET=24 ,SCR_XSIZE=30 ,SCR_YSIZE=24 ,/EDITABLE  $
      ,/ALL_EVENTS ,XSIZE=20 ,YSIZE=1)


  LBL_I_col = Widget_Label(BSE_ALS_SPECTRA, UNAME='LBL_I_col'  $
      ,XOFFSET=406 ,YOFFSET=3 ,SCR_XSIZE=34 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='col. #')


  LBL_cols_header = Widget_Label(BSE_ALS_SPECTRA,  $
      UNAME='LBL_cols_header' ,XOFFSET=555 ,/ALIGN_CENTER  $
      ,VALUE='DATA COLUMNS')


  LBL_cols_1 = Widget_Label(BSE_ALS_SPECTRA, UNAME='LBL_cols_1'  $
      ,XOFFSET=550 ,YOFFSET=20 ,SCR_XSIZE=94 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='1. OSA')


  LBL_cols_2 = Widget_Label(BSE_ALS_SPECTRA, UNAME='LBL_cols_2'  $
      ,XOFFSET=550 ,YOFFSET=38 ,SCR_XSIZE=122 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='2. Transmission')


  LBL_cols_3 = Widget_Label(BSE_ALS_SPECTRA, UNAME='LBL_cols_3'  $
      ,XOFFSET=549 ,YOFFSET=55 ,SCR_XSIZE=126 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='3. Other (e.g. TEY)')


  TXT_Io_col = Widget_Text(BSE_ALS_SPECTRA, UNAME='TXT_Io_col'  $
      ,XOFFSET=409 ,YOFFSET=77 ,SCR_XSIZE=30 ,SCR_YSIZE=24 ,/EDITABLE  $
      ,/ALL_EVENTS ,XSIZE=20 ,YSIZE=1)


  TBL_I = Widget_Table(BSE_ALS_SPECTRA, UNAME='TBL_I' ,XOFFSET=164  $
      ,YOFFSET=190 ,COLUMN_LABELS=[ 'value' ] ,ROW_LABELS=[ 'dwell'+ $
      ' (ms)', 'E_min', 'E_max', '#sects', '# cols', '#dark', 'Avg'+ $
      ' dark' ] ,XSIZE=1 ,YSIZE=7)


  TBL_Io = Widget_Table(BSE_ALS_SPECTRA, UNAME='TBL_Io' ,XOFFSET=347  $
      ,YOFFSET=190 ,COLUMN_LABELS=[ 'value' ] ,ROW_LABELS=[ 'dwell'+ $
      ' (ms)', 'E_min', 'E_max', '#sects', '# cols', '#dark', 'Avg'+ $
      ' dark' ] ,XSIZE=1 ,YSIZE=7)


  WID_LABEL_0 = Widget_Label(BSE_ALS_SPECTRA, UNAME='WID_LABEL_0'  $
      ,XOFFSET=165 ,YOFFSET=167 ,/ALIGN_LEFT ,VALUE='Sample scan'+ $
      ' parameters')


  WID_LABEL_1 = Widget_Label(BSE_ALS_SPECTRA, UNAME='WID_LABEL_1'  $
      ,XOFFSET=343 ,YOFFSET=167 ,/ALIGN_LEFT ,VALUE='Reference scan'+ $
      ' parameters')

  Widget_Control, /REALIZE, BSE_ALS_SPECTRA

  XManager, 'BSE_ALS_SPECTRA', BSE_ALS_SPECTRA

end
;
; Empty stub procedure used for autoloading.
;
pro als_spectra, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  BSE_ALS_SPECTRA, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end

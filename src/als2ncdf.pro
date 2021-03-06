; Copyright (c) 1998-2000 A.P. Hitchcock  All rights reserved
;+
;NAME:
;	ALS2NCDF
;
;LAST CHANGED: ----------------------------------- 27-feb-00
;
; PURPOSE:
;	This procedure converts one or a series of ALS image files
; into netCDF format and, if multiple files, writes a stack_list (*.sl) file
; with names of converted files
;
; CATEGORY:
;	AXIS: image analysis
;
; CALLING SEQUENCE:
;	ALS2NCDF, file
;
; INPUTS:
;	FILE - an ALS image file (*.im%) to be converted to netCDF format (*.nc)
;
; KEYWORD PARAMETERS:
;	LIST - name of file with list of image files (*.lst)
;	BIN - average by combining 2x2 pixels (bin=2) or 3x3 pixels (bin=3), etc
;   OUTLIST - name of a file to write list of netCDF files (*.sl)
;   DELE - delta_E is a photon energy shift (real - measured) (eV)
;	ONE - convert only a single file
;
; OUTPUTS:
;  FILE.NC - image written to a netCDF format binary file
;
; COMMON BLOCKS:
;   @AXIS_COM - set of common blocks for AXIS
;   @BSIF_COM - common block for netCDF
;
; SIDE EFFECTS:
;	many !
;
; RESTRICTIONS:
;	caveat emptor
;
; PROCEDURE:
;	ALS2NCDF assumes that ax_rd_im has put the image and associated information
; into BSIF_COMMON variables:
;	image_data (the array which holds the image - possibly several
;	planes deep for I, IO, CLOCK)
;
; EXAMPLE:
;	ALS2NCDF is used from AXIS by supplying user-defined keywords
;
;  fileget=PICKFILE2(/Read, FILTER='*.lst', /LPATH, DEFPATH=defpath)
;  if strlen(fileget) GT 0 THEN BEGIN
;  		WIDGET_CONTROL, Uprompt, SET_VALUE= 'ALS->nc: smooth by binning?'
;		bin_num=1				; optional binning of data
;		bin_num=get_num(Val=bin_num,PROMPT='bin *', group=axis_ID)
;		WIDGET_CONTROL, Uprompt, Bad_ID = badID, SET_VALUE= 'ALS->nc: calibrate E?'
;		del_E=0.
;		del_E = get_num(Val=del_E,PROMPT='dE(real-meas, eV)', group=axis_ID)
;		sl_file = get_text(Prompt = 'stack_list file',val = sl_file, group=axis_ID)
;	    ALS2NCDF,list=fileget,bin=bin_num, outlist=sl_file, delE=del_E
;  endif
;
; MODIFICATION HISTORY:
; (xx-mar-98 aph) developed to support stacks at ALS
; (14-jun-98 aph) axis_com, bsif_com (for Linux idl 5.03)
; ( 2-aug-98 aph) define extension which is the image (switches !)
; (16-aug-98 aph) allow use of *.lst files generated by ALS stxm.vi
; (20-nov-98 aph) get rid of IDL command window prompt
; ( 7-jan-99 aph) sorting out problem with multi-file processing (stalls)
; (12-jan-99 aph) adapt to find data files when using ALS *.lst paths are wrong
; (14-apr-99 aph) DataPath default to Defath
; (13-May-99 aph) conditional plotting at end if 1 file
; ( 6-jul-99 aph) free_lun NOT just close !
; (11-aug-99 aph) fix up path issues;
; (05-sep-99 aph) write to real netcdf
; (19-sep-99 aph) write outlist file in ZIMBA format
; (15-oct-99 aph) correct path information at top oz Zimba format
; (30-dec-99 aph) AXIS standard documentation; add ONE keyword
; (27-feb-00 aph) groupID added to get_text
; (18-jul-01 aph) fixe error in optional /one and /list options
;-

pro als2ncdf, file , list=list, bin=bin, outlist = outlist, one = one, delE=delE

@axis_com
@bsif_com
on_error,2

if keyword_set(list) then begin
   osname=strupcase(strmid(!version.os,0,3))
   stack_readlist, list, filename_list
endif else begin
	if n_elements(file) eq 0 then begin  ;popup file dialog box
	   if keyword_set(one) then begin
		   filename_list = PICKFILE2(/Read, FILTER='*.im*', title = 'Select ALS image file', /LPATH, DEFPATH=defpath)
		endif else begin
	 		list=PICKFILE2(/Read, FILTER='*.lst', title = 'Select list file', /LPATH, DEFPATH=defpath)
	 		stack_readlist, list, filename_list
		endelse
	endif else filename_list = file
endelse
print, 'files to be read are ', filename_list
n_files = n_elements(filename_list)
if keyword_set(delE) then del_E = float(delE) else del_E=0.

; -------- use GET_PATH to establish correct file names
; -------- check if file exists - this handles subdirectory changes
file1 = filename_list(0)
New_Path = get_path(file1, list = list)
if New_Path EQ '' then return	; let user cancel
for i = 0, n_files-1 do begin
	t = ax_name(filename_list(i))
	filename_list(i) = New_Path + t(1) + '.' + t(2)
endfor

; ------ open *.sl file for stack_list -----------------
if keyword_set(outlist) then begin
; -------- force outlist to have .sl extension -----------------
	t = ax_name(outlist)
	outlist = t(0) + t(1) + '.sl'
	openw,unitw, outlist,/get_lun
	printf, unitw, strcompress(New_Path)		; write path at top of file
endif

; -------------- check back for user to see if filename list is valid---------
	t = ax_name(filename_list(0))
	print, 'First file to process is ', t(0) + t(1) + '.' + t(2)

FOR i_file = 0,n_files-1 DO BEGIN
	WIDGET_CONTROL, /hourglass
	file = filename_list(i_file)
	IF strlen(file) GT 0 THEN BEGIN
;	print,' Procesing file ', fix(i_file), '. . . ', file
		t = ax_name(file)
		ext = t(2)
		fileshort = t(1)
		Data_path = t(0)
		file = t(0) + t(1)
; READ IN DATA
		if keyword_set(bin) then ax_rd_im, file, bin=bin, img=ext  $
		else ax_rd_im, file, img=ext      ; use jacobsen's read als image routine
		energy = 12398.0/sd.wavelength + del_E(0)
		sd.wavelength = 12398.0/energy
;		print, Format = '(A, "  E=",F7.3, "(eV).  Dwell=", f4.2 ,"(ms)")', $
;			file, energy, sd.dwell_time
		data_title = byte(fileshort)
		x_title = 'X'
		y_title = 'Y'
		image_title = byte(fileshort)
		sd.clock_frequency = 1.
; WRITE-OUT DATA
		file = t(0) + t(1) + '.nc'    ; setup correct name for output in nc format
		wrstx_ax,file, /real
		if keyword_set(outlist) then printf,unitw, strcompress(t(1) + '.nc')
		print,'wrote nsls NetCDF format file: ', file
		WIDGET_CONTROL,UPrompt, Bad_ID = badID, SET_VALUE='ALS_to_netCDF '+ fileshort $
		   + '       E(eV)= '+strcompress(string(energy))
	ENDIF
ENDFOR

if keyword_set(outlist) then begin
	close, unitw
	free_lun, unitw
	print, 'wrote stack_list file: ', outlist
	WIDGET_CONTROL, Uprompt, Bad_ID = badID, $
	    SET_VALUE='stack_list saved in ' + strcompress(outlist)
endif
close,/all		; useful to ensure logical units are available !

if NOT keyword_set(list) then begin	; plot if only 1 file converted
	t = Widget_info(/active)	; BUT only if AXIS is running
	if t NE 0 then begin
		tmp = read_bnl(file)
		Label(CurBuf) = tmp.dl
		HANDLE_VALUE, Data(CurBuf), tmp, /set
		PlotBuf, CurBuf
	endif
endif
END

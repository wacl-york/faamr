CDF       
      Time   <         comment      �
FAAM data core_faam_yyyymmdd_vnnn_rn_cnnn.nc

 where yyyymmdd is the flight start date, vnnn the processing version cnnn is
 the flight number and rn the revision number.

The NetCDF file comprises a header followed by the actual data.  The header
contains a list of the parameters in the dataset - their long names, short
names, units, and measurement frequency.  For each named parameter there is a
matching parameter with FLAG added to the parameter name which contains
details of the quality of each measurement.  The FLAG parameters take four
possible values:

0 - measurement believed to be good.
Other flags need to be checked in the metadata for each measurement, which
is stored at the BADC

The NetCDF file header also contains:

1.      Flight number
2.      Flight date
3.      Data date - date datafile was created
4.      History - contains information about the data processing, not all of
        which may be relevant to the data in the final NetCDF file:
        a.      The dataset the NetCDF file was produced from
        b.      Input and output files used by the calibration process
        c.      The versions of the software modules used by the calibration
                process
        d.      Processor information
        e.      Constants used for calibration
        f.      Processing modules used for calibration
        g.      Data start and end times
        h.      Raw input parameters to the calibration process
        i.      Calibrated output parameters to the calibration process
        j.      Summary of quality flags applied by the calibration process
        k.      Details of other data added to, or taken from, the original
                calibrated dataset.
     	Data_Date         20191115   files         �PRTAFT:CRIO
CHTSOO:CRIO
AERACK:CRIO
CHFGGA:CRIO
UPPBBR:CRIO
TWBOZO:CRIO
CORCON:CRIO
AL52CO:CRIO
CPC378:CRIO
LOWBBR:CRIO
flight-cst_faam_20190711_r1_c179.txt:CONST
core_faam_20190711_r0_c179_rawdlu.zip:ZIP
GINDAT:GINDAT
WVSS2A:WVSS2A
WVSS2B:WVSS2B
    SoftwareRepository        $https://github.com/ncasuk/decades-pp   format_version        1.0    Title         Data from c179 on 11-Jul-2019      RosemountTemperatureThreshold               Flight_Constants     �!
! FLIGHT c179
! DATE 11 July 2019
!
GIN_HDG_OFFSET  0.0      ! The GIN is not always aligned perfectly
GIN_TIME_OFFSET 0.0      ! in secs; accounts for GPS-time setup of the GIN
!
!c_airspd  Airspeed correction factor
TASCORR   1.00000E+0
!
!c_drs    Cabin temperature (deg C)
CALCABT  -2.638669E+02	1.532049E-04
!
!            const        x
!c_geneas  General Eastern hygrometer
GELIMS    7000  5000  !max, min
CALGE    -7.827078E+01 4.194404E-03  0.00000E+0 ! 25514
!
!c_heiman  RADTHERM calibration constants
PRTCCAL  -2.225538E+01	1.922331E-03   0.00000E+0 ! Celsius  
HEIMCAL  -4.530669E+01	3.036188E-03   0.00000E+0 ! Celsius  
!
!c_ins1    INU level offsets - roll, pitch, yaw - deg
INSLEVL   0.00000E+0  0.00000E+0  0.00000E+0
!
!c_nevz    Nevzorov vane SN380 15/03/2019
! Types
!
VANETYPE    1T2L1R
CLWCIREF    -5.774363E-02   3.289288E-04  5.0e-01
CLWCVREF    -5.778096E-02   3.289289E-04  2.0
CLWCICOL    -5.779572E-02   3.289273E-04  5.0e-01
CLWCVCOL    -5.770866E-02   3.289263E-04  2.0
CTWCIREF    -5.771193E-02   3.289273E-04  5.0e-01
CTWCVREF    -5.770442E-02   3.289268E-04  2.0
CTWCICOL    -5.771335E-02   3.289274E-04  5.0e-01
CTWCVCOL    -5.789432E-02   3.289287E-04  2.0

CALNVLWC1    1.35  0.258e-4
CALNVLWC2    1.95  0.381e-4
CALNVTWC     0.70  0.502e-4
CALNVL      2589.0

!
!            const        x
!c_ozone1  Teco Ozone
CALO3    -1.227630E+0  1.896381E-4 0.00000E+0  5.20000E+2 !calibration coefficient changed 14_01_2011
CALO3P   -1.198721E+0  1.896757E-4 5.2589E+0   135.5400
CALO3T   -1.191110E+0  1.893918E-4 -18.970 24.209 -4.4547 0.4497 -0.0175
CALO3F   -1.207855E+0  1.895840E-4   !Flow drs to volts DRS 12_12_07
CALO3MX   -5.0 300.0 500.0 50.0  !Min Max flag 2 Max and min airspeed
!
!c_comr
CALCOMR   -1.193742E+0  1.896764E-4 1000.0 0.0 !Drs to volts, v to ppb, ppb offset
CALCOMX   -10.0 100000.0 50.0 1.0  !Min Max values and min airspeed
!
!c_press1 - Cabin and S9 pressure transducers, X0, X1, X2
CALCABP  2.757904E+00 3.293439E-02 -2.462971E-10 ! #1584
CALS9SP  -1.30655557e+02  3.71198752e-02  1.84877706e-09
!
!c_psap    PSAP
CALPLIN  -1.180833E+0  1.893540E-4 ! DRS coeffs only 12_12_07
CALPLOG  -1.189457E+0  1.894231E-4 ! DRS coeffs only
!
! BROAD BAND RADIOMETERS
!
!
!c_sols    BBR's - Sensitivity X0, X1, T0, T1
CALUP1S   5.768856E+0  4.986336E-2  1.282223E+2 -6.037012E-3 ! H394
CALUP2S   1.268199E+0  2.933988E-2  1.280583E+2 -6.042037E-3 ! H632
CALUIRS   -1.464148E+3  9.500935E-2  1.341723E+2 -6.021609E-3 ! H498 
CALLP1S   6.622763E-1  4.315156E-2  1.288084E+2 -6.032263E-3 ! H631
CALLP2S   2.438363E-1  2.674311E-2  1.285021E+2 -6.034057E-03 ! H392 
CALLIRS   -5.347174E+2  3.442438E-2  1.338966E+2 -6.032221E-3 ! H497 
!
! Instrument serial numbers C=clear pyran. R=red pyran. I=pyrgeometer
!
!ISN 81  C394
!ISN 82  R392
!ISN 83  I498
!ISN 91  C490
!ISN 92  R495
!ISN 93  I497
!
! pitch and roll offsets changed post FENNEC pilot campaign after a recommendation
! from Claire Ryder, who analysed the pirouette and the box pattern from flight B597 27/05/2011
!c_rflux   Corrected BBR fluxes - TA,TB,TC,Pit,Rol,Obs (1=none 2=small 3=large)
CALCUCF   0.00000E-3  0.00000E+0  0.00000E+0 -2.80000E+0  0.30000E+0  1.00000E+0
CALCURF   0.00000E-3  0.00000E+0  0.00000E+0 -3.20000E+0 -0.10000E+0  1.00000E+0
CALCUIF  -2.00000E-3  0.00000E+0  0.00000E+0 -3.00000E+0  0.00000E+0  1.00000E+0
CALCLCF   0.00000E+0  0.00000E+0  0.00000E+0  0.00000E+0  0.00000E+0  1.00000E+0
CALCLRF   0.00000E+0  0.00000E+0  0.00000E+0  0.00000E+0  0.00000E+0  1.00000E+0
CALCLIF  -2.00000E-3  0.00000E+0  0.00000E+0  0.00000E+0  0.00000E+0  1.00000E+0
!
!            const        x
!c_temps2  Recovery factors and coeffs for deiced and non-deiced - deg C
TRFCTR   9.9280E-1  9.9900E-1 
DITSENS  20472E plate
NDTSENS  19205E thermistor
CALNDT   0.            0.            0.                    ! 19205E thermistor 24/06/2019
CALDIT  -2.428784E+02  3.763738E-05  3.167788E-13          ! 20472E plate 24/06/2019
!
!c_tpress - Turbulence probe transducers, X0, X1, X2, X3
CALTP1  8.854721E-1 5.444868E-03 1.981422E-9 -3.628851E-14 ! 2159 cal 16/11/18
CALTP2  -1.422369E-1 2.101471E-5 1.998357E-15 -5.641362E-23 ! 2601 cal 16/11/18
CALTP3  -7.290181E-01 2.101177E-05 1.565457E-15 5.574456E-22   ! 2935 cal 16/11/18
CALTP4  1.514184E+00 1.081172E-02
CALTP5  -1.192508E+00 1.070300E-02
!
!c_turb     Turbulence probe flow angle and TAS calculations, X0, X1, X1
AOA_A0    3.35361E-01  2.78277E-01 -5.73689E-01
AOA_A1   -6.1619E-02 -5.2595E-02  1.0300E-01
AOSS_B0  -2.1887E-02  0.0000E-00  0.0000E0 ! Recalculated 23_1_07
AOSS_B1   5.7967E-02 -1.7229E-02  0.0000E0 ! Recalculated 23_1_07
TOLER     2.0000E-03
TASCOR1   1.008E0   ! value fitted manually 07/05/2016
ALPH0     -0.4126E+0
ALPH1     1.0776E+0
BET0      -0.70E+0 ! Phil Brown 2/3/09
BET1      0.9505E+0
!
! p_rio_so2
SO2_SENS 1.15
!
!c_twc     Lyman alpha total water - temps in K, currents in A, TWC decades refit 22/10/2013
CALTNOS   1.2614E-16 -1.8668E-12  1.2704E-08  9.4262E-03  3.1654E+02
CALTSAM   10.88318003  3894.52652733  9267.64142513  7824.80596698
CALTAMB   2.9951E-13  9.3288E-10 -4.0779E-06  1.6016E-02  2.7390E+02
CALTSRC   2.8700E-18 -1.2794E-14  2.8480E-11  2.2585E-10  9.5178E-03  3.7298E+02
CALHTR1   9.7752E-04  0.0000E+00
CALHTR2   9.7752E-04  0.0000E+00
CALISRC   9.7636E-08 -2.5957E-06
!
!c_winds   Position of vanes wrt INS/INU - forward, port, up - m
INSPOSN  16.00200E+0 -0.81280E+0 -0.43900E+0
!
!buck mirror temperature calibration coefficients
BUCK      0.00000  1.0000 ! M-219 
! Data Processing modules to be run
! ---------------------------------
NOCNC
!CLOUDP    !updated
!CPC
!GENEAS
!GPS       !XR5M V5.02
!HEIMAN
!INS1        !Leave in (although INU removed Mar09)
!LWC      !Johnson Williams
!NEPHL1  !removed on 07/03/2012#b676
!NONEVZ
NOSO2
!OZONE1   !updated
!COMR     !updated
NONOX     !no more NOX to be fitted
!PRESS1
!PSAP    !PSAP data recorded via DLU & HORACE - also backed up on PSAP laptop
!RVSM
!RADAL1
!SOLS     !ie BBRs
!TEMPS2
!TPRESS
!TURB
!TWC     !Lyman Alpha, Updated Both TWC Probes use the same constants
!WINDS
! Revision 0 - 11 July 2019
     Coordinates       LON_GIN LAT_GIN ALT_GIN Time   Conventions       CF-1.4     SoftwareVersion       1.1    
references        http://www.faam.ac.uk      history      5�
READCONST
  Inputs=[CONST] ,
  Outputs=[Flight_Constants, FLIGHT, DATE, GIN_HDG_OFFSET, GIN_TIME_OFFSET, TASCORR, CALCABT, GELIMS, CALGE, PRTCCAL, HEIMCAL, INSLEVL, VANETYPE, CLWCIREF, CLWCVREF, CLWCICOL, CLWCVCOL, CTWCIREF, CTWCVREF, CTWCICOL, CTWCVCOL, CALNVLWC1, CALNVLWC2, CALNVTWC, CALNVL, CALO3, CALO3P, CALO3T, CALO3F, CALO3MX, CALCOMR, CALCOMX, CALCABP, CALS9SP, CALPLIN, CALPLOG, CALUP1S, CALUP2S, CALUIRS, CALLP1S, CALLP2S, CALLIRS, CALCUCF, CALCURF, CALCUIF, CALCLCF, CALCLRF, CALCLIF, TRFCTR, DITSENS, NDTSENS, CALNDT, CALDIT, CALTP1, CALTP2, CALTP3, CALTP4, CALTP5, AOA_A0, AOA_A1, AOSS_B0, AOSS_B1, TOLER, TASCOR1, ALPH0, ALPH1, BET0, BET1, SO2_SENS, CALTNOS, CALTSAM, CALTAMB, CALTSRC, CALHTR1, CALHTR2, CALISRC, INSPOSN, BUCK] 


READZIPPED
  Inputs=[ZIP] ,
  Outputs=[WVSS2B, CRIO, GINDAT, WVSS2A] 


READ_GINDAT
  Inputs=[GINDAT, DATE] ,
  Outputs=[GINDAT_packet_length, GINDAT_time1, GINDAT_time2, GINDAT_dist, GINDAT_timetype, GINDAT_disttype, GINDAT_lat, GINDAT_lon, GINDAT_alt, GINDAT_veln, GINDAT_vele, GINDAT_veld, GINDAT_roll, GINDAT_ptch, GINDAT_hdg, GINDAT_wand, GINDAT_trck, GINDAT_gspd, GINDAT_rolr, GINDAT_pitr, GINDAT_hdgr, GINDAT_aclf, GINDAT_acls, GINDAT_acld, GINDAT_status, GINDAT_checksum, GINDAT_grpend] 


READ_CRIOX
  Inputs=[CRIO, DATE] ,
  Outputs=[PRTAFT_packet_length, PRTAFT_utc_time, PRTAFT_ptp_sync, PRTAFT_crio_temp, PRTAFT_flight_num, PRTAFT_crio_fpga_run, PRTAFT_crio_rtc_run, PRTAFT_crio_status, PRTAFT_crio_UDP_num, PRTAFT_crio_mem_used, PRTAFT_crio_disktype, PRTAFT_crio_aux_out3, PRTAFT_pressure_alt, PRTAFT_ind_air_speed, PRTAFT_rad_alt, PRTAFT_unused_1, PRTAFT_unused_2, PRTAFT_deiced_temp_flag, PRTAFT_wow_flag, PRTAFT_heimann_calib_flag, PRTAFT_jci140_signal, PRTAFT_jci140_range, CHTSOO_packet_length, CHTSOO_utc_time, CHTSOO_ptp_sync, CHTSOO_dummy, CHTSOO_flight_num, CHTSOO_conc, CHTSOO_pmt_volt, CHTSOO_lamp_volt, CHTSOO_lamp_int, CHTSOO_internal_temp, CHTSOO_react_temp, CHTSOO_react_press, CHTSOO_flow, CHTSOO_flags, CHTSOO_MFM, CHTSOO_ten_min_so2, CHTSOO_eight_hr_so2, CHTSOO_ten_min_so2_alm, CHTSOO_eight_hr_so2_alm, CHTSOO_MFC2_absolute_pressure, CHTSOO_MFC2_temperature, CHTSOO_MFC2_volumetric_flow, CHTSOO_MFC2_mass_flow, CHTSOO_MFC2_set_point, CHTSOO_MFC3_absolute_pressure, CHTSOO_MFC3_temperature, CHTSOO_MFC3_volumetric_flow, CHTSOO_MFC3_mass_flow, CHTSOO_MFC3_set_point, CHTSOO_V6, CHTSOO_V8, CHTSOO_V9, CHTSOO_MFC2_Set_Value, CHTSOO_MFC3_Set_Value, CHTSOO_mean_zero_cal_status, CHTSOO_zero, CHTSOO_sensitivity, CHTSOO_ev8_user_set_time, CHTSOO_auto_zeroing, CHTSOO_ev8_zero_interval, CHTSOO_inflight_alarm_MOCCA, CHTSOO_MOCCA_so2_time, CHTSOO_MOCCA_so2_limit, CHTSOO_ten_min_STEL_limit, CHTSOO_eight_hr_TWA_limit, CHTSOO_cal_conc, CHTSOO_simulate_conc, CHTSOO_mocca_so2_sim, CHTSOO_mocca_so2_alm, AERACK_packet_length, AERACK_utc_time, AERACK_ptp_sync, AERACK_crio_temp, AERACK_flight_num, AERACK_crio_fpga_run, AERACK_crio_rtc_run, AERACK_crio_status, AERACK_crio_UDP_num, AERACK_crio_mem_used, AERACK_crio_disktype, AERACK_crio_aux_out3, AERACK_filter_1_flow, AERACK_filter_1_pressure, AERACK_filter_2_flow, AERACK_filter_2_pressure, AERACK_psap_flow, AERACK_psap_lin, AERACK_psap_log, AERACK_psap_transmission, AERACK_neph_total_blue, AERACK_neph_total_green, AERACK_neph_total_red, AERACK_neph_backscatter_blue, AERACK_neph_backscatter_green, AERACK_neph_backscatter_red, AERACK_neph_pressure, AERACK_neph_temp, AERACK_neph_humidity, AERACK_neph_status, AERACK_neph_inst_time, AERACK_neph_mode, AERACK_buck_ppm, AERACK_buck_cham_temp, AERACK_buck_mirr_temp, AERACK_buck_dewpoint_flag, AERACK_buck_coldfinger_temp, AERACK_buck_pressure, AERACK_buck_balance, AERACK_buck_pwm, AERACK_buck_mirr_cln_flag, AERACK_buck_board_temp, AERACK_buck_input_volts, CHFGGA_packet_length, CHFGGA_utc_time, CHFGGA_ptp_sync, CHFGGA_dummy2, CHFGGA_flight_num, CHFGGA_ch4, CHFGGA_co2, CHFGGA_h2o, CHFGGA_press_tor, CHFGGA_temp_c, CHFGGA_fit_flag, CHFGGA_rda_usec, CHFGGA_rdb_usec, CHFGGA_ch4_dry, CHFGGA_co2_dry, CHFGGA_MFM, CHFGGA_MFC_1_absolute_pressure, CHFGGA_MFC_1_temperature, CHFGGA_MFC_1volumetic_flow, CHFGGA_MFC_1mass_flow, CHFGGA_MFC_1set_point, CHFGGA_V1, CHFGGA_V2, CHFGGA_V3, CHFGGA_V4, CHFGGA_restart_FGGA, CHFGGA_FGGA_Pump, CHFGGA_CAL_MFC_1Set_Value, UPPBBR_packet_length, UPPBBR_utc_time, UPPBBR_ptp_sync, UPPBBR_crio_temp, UPPBBR_flight_num, UPPBBR_crio_fpga_run, UPPBBR_crio_rtc_run, UPPBBR_crio_status, UPPBBR_crio_UDP_num, UPPBBR_crio_mem_used, UPPBBR_crio_disktype, UPPBBR_crio_aux_out3, UPPBBR_radiometer_1_sig, UPPBBR_radiometer_1_temp, UPPBBR_radiometer_2_sig, UPPBBR_radiometer_2_temp, UPPBBR_radiometer_3_sig, UPPBBR_radiometer_3_temp, UPPBBR_radiometer_4_sig, UPPBBR_radiometer_4_temp, UPPBBR_radiometer_1_zero, UPPBBR_radiometer_2_zero, UPPBBR_radiometer_3_zero, UPPBBR_radiometer_4_zero, TWBOZO_packet_length, TWBOZO_utc_time, TWBOZO_ptp_sync, TWBOZO_dummy, TWBOZO_flight_num, TWBOZO_MFM, TWBOZO_conc, TWBOZO_temp, TWBOZO_press, TWBOZO_flow, CORCON_packet_length, CORCON_utc_time, CORCON_ptp_sync, CORCON_crio_temp, CORCON_flight_num, CORCON_crio_fpga_run, CORCON_crio_rtc_run, CORCON_crio_status, CORCON_crio_UDP_num, CORCON_crio_mem_used, CORCON_crio_disktype, CORCON_crio_aux_out3, CORCON_tp_up_down, CORCON_tp_left_right, CORCON_fast_temp, CORCON_padding1, CORCON_ndi_temp, CORCON_di_temp, CORCON_padding2, CORCON_padding3, CORCON_tp_p0_s10, CORCON_tp_top_s10, CORCON_tp_right_s10, CORCON_s9_press, CORCON_nv_lwc_iref, CORCON_nv_lwc_vref, CORCON_nv_lwc_icol, CORCON_nv_lwc_vcol, CORCON_nv_twc_iref, CORCON_nv_twc_vref, CORCON_nv_twc_icol, CORCON_nv_twc_vcol, CORCON_cabin_p, CORCON_cabin_t, CORCON_fasttemp_hi_lo, CORCON_padding4, CORCON_heim_t, CORCON_heim_c, CORCON_ge_dew, CORCON_ge_cont, CORCON_jw_lwc, CORCON_padding5, CORCON_padding6, CORCON_padding7, AL52CO_packet_length, AL52CO_utc_time, AL52CO_ptp_sync, AL52CO_ue9LJ_temp, AL52CO_flight_num, AL52CO_conc, AL52CO_counts, AL52CO_sens, AL52CO_zero, AL52CO_lampflow, AL52CO_lamptemp, AL52CO_monoflow, AL52CO_monotemp, AL52CO_monopress, AL52CO_cellpress, AL52CO_calpress, AL52CO_temppmt, AL52CO_calconc, AL52CO_err, AL52CO_cal_status, AL52CO_MFM, AL52CO_CO_Cal, AL52CO_cal_counter, AL52CO_err_counter, AL52CO_CO_vacuum_status, AL52CO_ALREG1_press_delivery, AL52CO_ALREG1_press_content, AL52CO_ALREG2_press_content, AL52CO_ALREG3_press_content, CPC378_packet_length, CPC378_utc_time, CPC378_ptp_sync, CPC378_crio_temp, CPC378_flight_num, CPC378_sample_time, CPC378_counts, CPC378_live_time, CPC378_sample_flow, CPC378_total_flow, CPC378_sheath_flow, CPC378_pressure, CPC378_saturator_temp, CPC378_growth_tube_temp, CPC378_optics_temp, LOWBBR_packet_length, LOWBBR_utc_time, LOWBBR_ptp_sync, LOWBBR_crio_temp, LOWBBR_flight_num, LOWBBR_crio_fpga_run, LOWBBR_crio_rtc_run, LOWBBR_crio_status, LOWBBR_crio_UDP_num, LOWBBR_crio_mem_used, LOWBBR_crio_disktype, LOWBBR_crio_aux_out3, LOWBBR_radiometer_1_sig, LOWBBR_radiometer_1_temp, LOWBBR_radiometer_2_sig, LOWBBR_radiometer_2_temp, LOWBBR_radiometer_3_sig, LOWBBR_radiometer_3_temp, LOWBBR_radiometer_4_sig, LOWBBR_radiometer_4_temp, LOWBBR_radiometer_1_zero, LOWBBR_radiometer_2_zero, LOWBBR_radiometer_3_zero, LOWBBR_radiometer_4_zero] 


RIO_SOLS
  Inputs=[CALUP1S, CALUP2S, CALUIRS, CALLP1S, CALLP2S, CALLIRS, SECS, UPPBBR_radiometer_1_sig, UPPBBR_radiometer_2_sig, UPPBBR_radiometer_3_sig, UPPBBR_radiometer_1_zero, UPPBBR_radiometer_2_zero, UPPBBR_radiometer_3_zero, UPPBBR_radiometer_1_temp, UPPBBR_radiometer_2_temp, UPPBBR_radiometer_3_temp, LOWBBR_radiometer_1_sig, LOWBBR_radiometer_2_sig, LOWBBR_radiometer_3_sig, LOWBBR_radiometer_1_zero, LOWBBR_radiometer_2_zero, LOWBBR_radiometer_3_zero, LOWBBR_radiometer_1_temp, LOWBBR_radiometer_2_temp, LOWBBR_radiometer_3_temp] ,
  Outputs=[UP1S, UP2S, UIRS, UP1Z, UP2Z, UIRZ, UP1T, UP2T, UIRT, LP1S, LP2S, LIRS, LP1Z, LP2Z, LIRZ, LP1T, LP2T, LIRT] 


GIN
  Inputs=[DATE, GINDAT_time1, GINDAT_lat, GINDAT_lon, GINDAT_alt, GINDAT_veln, GINDAT_vele, GINDAT_veld, GINDAT_roll, GINDAT_ptch, GINDAT_hdg, GINDAT_wand, GINDAT_trck, GINDAT_gspd, GINDAT_rolr, GINDAT_pitr, GINDAT_hdgr, GINDAT_aclf, GINDAT_acls, GINDAT_acld, GINDAT_status] ,
  Outputs=[LAT_GIN, LON_GIN, ALT_GIN, VELN_GIN, VELE_GIN, VELD_GIN, ROLL_GIN, PTCH_GIN, HDG_GIN, WAND_GIN, TRCK_GIN, GSPD_GIN, ROLR_GIN, PITR_GIN, HDGR_GIN, ACLF_GIN, ACLS_GIN, ACLD_GIN, SECS_GIN] 


GSUN
  Inputs=[DATE, SECS, LAT_GIN, LON_GIN] ,
  Outputs=[SOL_AZIM, SOL_ZEN] 


RIO_SUN
  Inputs=[DATE, SECS_GIN, LAT_GIN, LON_GIN] ,
  Outputs=[SOL_AZIM, SOL_ZEN] 


GRFLUX
  Inputs=[CALCUCF, CALCURF, CALCUIF, CALCLCF, CALCLRF, CALCLIF, UP1S, UP2S, UIRS, UP1Z, UP2Z, UIRZ, UP1T, UP2T, UIRT, LP1S, LP2S, LIRS, LP1Z, LP2Z, LIRZ, LP1T, LP2T, LIRT, SOL_AZIM, SOL_ZEN, ROLL_GIN, PTCH_GIN, HDG_GIN] ,
  Outputs=[SW_DN_C, RED_DN_C, IR_DN_C, SW_UP_C, RED_UP_C, IR_UP_C] 


RIO_WEIGHT_ON_WHEELS
  Inputs=[PRTAFT_utc_time, PRTAFT_wow_flag] ,
  Outputs=[WOW_IND] 


RIO_NEPHELOMETER
  Inputs=[AERACK_neph_total_blue, AERACK_neph_total_green, AERACK_neph_pressure, AERACK_neph_temp, AERACK_neph_backscatter_blue, AERACK_neph_backscatter_green, AERACK_neph_backscatter_red, AERACK_neph_total_red, AERACK_neph_humidity, AERACK_neph_status, AERACK_neph_mode] ,
  Outputs=[NEPH_PR, NEPH_T, NEPH_RH, TSC_BLUU, TSC_GRNU, TSC_REDU, BSC_BLUU, BSC_GRNU, BSC_REDU] 


RIO_GENEAS
  Inputs=[GELIMS, CALGE, SECS, CORCON_ge_dew, CORCON_ge_cont] ,
  Outputs=[TDEW_GE] 


CALC_VP
  Inputs=[TDEW_GE] ,
  Outputs=[GE_VP_W, GE_VP_I, GE_VP] 


RIO_RVSM
  Inputs=[PRTAFT_pressure_alt, PRTAFT_ind_air_speed] ,
  Outputs=[PS_RVSM, Q_RVSM, PALT_RVS] 


RIO_FAST_THERMISTOR
  Inputs=[CORCON_fast_temp, CORCON_fasttemp_hi_lo, PS_RVSM, Q_RVSM, TRFCTR] ,
  Outputs=[R_FT, IAT_FT, TAT_FT] 


RIO_SREG
  Inputs=[CALCABT, PRTAFT_heimann_calib_flag, PRTAFT_deiced_temp_flag, CORCON_cabin_t] ,
  Outputs=[SREG, SREG_CAL, CAB_TEMP] 


RIO_TEMPS
  Inputs=[TRFCTR, CALDIT, CALNDT, CORCON_di_temp, SREG, CORCON_ndi_temp, SECS, PS_RVSM, Q_RVSM] ,
  Outputs=[ITDI, TAT_DI_R, NDTI, TAT_ND_R] 


AIRSPD
  Inputs=[TASCORR, SECS, PS_RVSM, Q_RVSM, TAT_DI_R] ,
  Outputs=[IAS_RVSM, TAS_RVSM] 


RIO_TPRESS
  Inputs=[CALTP1, CALTP2, CALTP3, CALTP4, CALTP5, CORCON_tp_p0_s10, CORCON_tp_up_down, CORCON_tp_left_right, CORCON_tp_top_s10, CORCON_tp_right_s10] ,
  Outputs=[P0_S10, PA_TURB, PB_TURB, TBPC, TBPD] 


TURB
  Inputs=[AOA_A0, AOA_A1, AOSS_B0, AOSS_B1, TOLER, TASCOR1, ALPH0, ALPH1, BET0, BET1, IAS_RVSM, TAT_DI_R, TAT_ND_R, PS_RVSM, Q_RVSM, PALT_RVS, P0_S10, PA_TURB, PB_TURB, TBPC, TBPD] ,
  Outputs=[AOA, AOSS, TAS, TASW, PSP_TURB] 


NOTURB_WINDVECTORS
  Inputs=[VELE_GIN, VELN_GIN, HDG_GIN, TAT_DI_R, TAS_RVSM, ROLL_GIN] ,
  Outputs=[U_NOTURB, V_NOTURB] 


GIN_TRACK_PLOT_CARTOPY
  Inputs=[DATE, FLIGHT, GINDAT_lat, GINDAT_lon] ,
  Outputs=[MAP_FILE] 


RIO_SO2_MIXINGRATIO
  Inputs=[CHTSOO_conc, CHTSOO_flags, CHTSOO_V6, CHTSOO_V8, WOW_IND] ,
  Outputs=[SO2_TECO] 


CALC_SVP
  Inputs=[TAT_DI_R] ,
  Outputs=[SAT_VP_W, SAT_VP_I] 


READ_WVSS2A
  Inputs=[WVSS2A, DATE] ,
  Outputs=[WVSS2F_VMR, WVSS2F_PRESS, WVSS2F_TEMP, WVSS2A_VMR_RAW, WVSS2A_PRESS_RAW, WVSS2A_TEMP_RAW, WVSS2A_PP2F, WVSS2A_LasSigPow, WVSS2A_PeakIndex, WVSS2A_NullValue, WVSS2A_MidPt, WVSS2A_AdjMidPt, WVSS2A_PT, WVSS2A_BLCF, WVSS2A_packet_length, WVSS2A_utc_time, WVSS2A_utc_time_msec, WVSS2A_ident, WVSS2A_flight_num, WVSS2A_serial_data] 


RIO_PYRGEOMETER
  Inputs=[LOWBBR_radiometer_3_sig, LOWBBR_radiometer_3_temp, UPPBBR_radiometer_3_sig, UPPBBR_radiometer_3_temp, WOW_IND] ,
  Outputs=[IR_DN_C, IR_UP_C] 


RIO_PSAP
  Inputs=[AERACK_psap_flow, AERACK_psap_lin, AERACK_psap_log, AERACK_psap_transmission] ,
  Outputs=[PSAP_LIN, PSAP_LOG, PSAP_FLO, PSAP_TRA] 


RIO_RADAL
  Inputs=[PRTAFT_rad_alt] ,
  Outputs=[HGT_RADR] 


RIO_CO_MIXINGRATIO
  Inputs=[AL52CO_conc, AL52CO_sens, AL52CO_zero, AL52CO_cellpress, AL52CO_calpress, AL52CO_cal_status, AL52CO_utc_time, AL52CO_counts, WOW_IND, HGT_RADR, CALCOMX] ,
  Outputs=[CO_AERO] 


READ_WVSS2B
  Inputs=[WVSS2B, DATE] ,
  Outputs=[WVSS2R_VMR, WVSS2R_PRESS, WVSS2R_TEMP, WVSS2B_VMR_RAW, WVSS2B_PRESS_RAW, WVSS2B_TEMP_RAW, WVSS2B_PP2F, WVSS2B_LasSigPow, WVSS2B_PeakIndex, WVSS2B_NullValue, WVSS2B_MidPt, WVSS2B_AdjMidPt, WVSS2B_PT, WVSS2B_BLCF, WVSS2B_packet_length, WVSS2B_utc_time, WVSS2B_utc_time_msec, WVSS2B_ident, WVSS2B_flight_num, WVSS2B_serial_data] 


RIO_HEIMAN
  Inputs=[PRTCCAL, HEIMCAL, SREG, CORCON_heim_t, CORCON_heim_c] ,
  Outputs=[BTHEIM_U] 


RIO_CPC
  Inputs=[CPC378_utc_time, CPC378_counts, CPC378_sample_flow, CPC378_total_flow, CPC378_sheath_flow, CPC378_pressure, CPC378_saturator_temp, CPC378_growth_tube_temp, CPC378_optics_temp] ,
  Outputs=[CPC_CNTS] 


RIO_PRESS
  Inputs=[CALCABP, CALS9SP, CORCON_cabin_p, CORCON_s9_press] ,
  Outputs=[CAB_PRES, P9_STAT] 


GWINDS
  Inputs=[INSPOSN, SECS_GIN, TAS, VELN_GIN, VELE_GIN, VELD_GIN, ROLL_GIN, PTCH_GIN, HDG_GIN, ROLR_GIN, PITR_GIN, HDGR_GIN, AOA, AOSS] ,
  Outputs=[V_C, U_C, W_C] 


ELECTRIC_FIELD_JCI140
  Inputs=[PRTAFT_jci140_signal, PRTAFT_jci140_range] ,
  Outputs=[EXX_JCI] 


RIO_BUCK_CR2
  Inputs=[BUCK, AERACK_buck_ppm, AERACK_buck_mirr_temp, AERACK_buck_pressure, AERACK_buck_dewpoint_flag, AERACK_buck_mirr_cln_flag, PS_RVSM] ,
  Outputs=[VMR_CR2, VMR_C_U, TDEW_CR2, TDEW_C_U, TDEWCR2C] 


RIO_NEVZOROV_1T2L1R
  Inputs=[CORCON_nv_lwc_vcol, CORCON_nv_lwc_icol, CORCON_nv_lwc_vref, CORCON_nv_lwc_iref, CORCON_nv_twc_vcol, CORCON_nv_twc_icol, CORCON_nv_twc_vref, CORCON_nv_twc_iref, TAS_RVSM, IAS_RVSM, PS_RVSM, WOW_IND, CLWCIREF, CLWCVREF, CLWCICOL, CLWCVCOL, CTWCIREF, CTWCVREF, CTWCICOL, CTWCVCOL, CALNVTWC, CALNVLWC1, CALNVLWC2, CALNVL] ,
  Outputs=[NV_TWC_U, NV_LWC1_U, NV_LWC2_U, NV_TWC_C, NV_LWC1_C, NV_LWC2_C, NV_TWC_P, NV_LWC1_P, NV_LWC2_P, NV_REF_P] 


RIO_2BOZONE
  Inputs=[TWBOZO_conc, TWBOZO_flow, WOW_IND] ,
  Outputs=[O3_2BTECH] 

; 1 minute of data extracted for testing purposes   institution       FAAM   revision             TimeInterval      04:12:55-11:43:12         �   Time                
_FillValue        ����   	long_name         time of measurement    standard_name         time   calendar      	gregorian      units         'seconds since 2019-07-11 00:00:00 +0000       �  �X   SOL_AZIM                
_FillValue        �<    number          �   	long_name         5Solar azimuth derived from aircraft position and time      standard_name         solar_azimuth_angle    	frequency               units         degree        �  �H   SOL_AZIM_FLAG                   
_FillValue        �      units         1      	long_name         >Flag for Solar azimuth derived from aircraft position and time     	frequency                  <  �8   SOL_ZEN                 
_FillValue        �<    number          �   	long_name         4Solar zenith derived from aircraft position and time   standard_name         solar_zenith_angle     	frequency               units         degree        �  �t   SOL_ZEN_FLAG                
_FillValue        �      units         1      	long_name         =Flag for Solar zenith derived from aircraft position and time      	frequency                  <  �d   IAS_RVSM                
_FillValue        �<    number             	long_name         <Indicated air speed from the aircraft RVSM (air data) system   standard_name                	frequency                units         m s-1         �  �   IAS_RVSM_FLAG                   
_FillValue        �      units         1      	long_name         EFlag for Indicated air speed from the aircraft RVSM (air data) system      	frequency                   <  �   TAS_RVSM                
_FillValue        �<    number             	long_name         NTrue air speed from the aircraft RVSM (air data) system and deiced temperature     standard_name         platform_speed_wrt_air     	frequency                units         m s-1         �  ��   TAS_RVSM_FLAG                   
_FillValue        �      units         1      	long_name         WFlag for True air speed from the aircraft RVSM (air data) system and deiced temperature    	frequency                   <  �   PA_TURB                 
_FillValue        �<    number             	long_name         HCalibrated differential pressure between turbulence probe vertical ports   standard_name                	frequency                units         hPa       �  ��   PA_TURB_FLAG                
_FillValue        �      units         1      	long_name         QFlag for Calibrated differential pressure between turbulence probe vertical ports      	frequency                   <  ��   PB_TURB                 
_FillValue        �<    number             	long_name         JCalibrated differential pressure between turbulence probe horizontal ports     standard_name                	frequency                units         hPa       �  �$   PB_TURB_FLAG                
_FillValue        �      units         1      	long_name         SFlag for Calibrated differential pressure between turbulence probe horizontal ports    	frequency                   <  �   TAT_DI_R                
_FillValue        �<    number             	long_name         [True air temperature from the Rosemount deiced temperature sensor (Type: plate; SN: 20472E)    standard_name         air_temperature    	frequency                units         degK      �  �P   TAT_DI_R_FLAG                   
_FillValue        �      units         1      	long_name         dFlag for True air temperature from the Rosemount deiced temperature sensor (Type: plate; SN: 20472E)   	frequency                   <  �@   PSAP_LOG                
_FillValue        �<    number          �   	long_name         ;Uncorrected absorption coefficient at 565nm, log, from PSAP    standard_name                	frequency               units         1         �  �|   PSAP_LOG_FLAG                   
_FillValue        �      units         1      	long_name         DFlag for Uncorrected absorption coefficient at 565nm, log, from PSAP   	frequency                  <  �l   PSAP_FLO                
_FillValue        �<    number               	long_name         	PSAP Flow      standard_name                	frequency               units         standard l min-1      �  �   PSAP_FLO_FLAG                   
_FillValue        �      units         1      	long_name         Flag for PSAP Flow     	frequency                  <  �   PSAP_TRA                
_FillValue        �<    number               	long_name         PSAP Transmittance     standard_name                	frequency               units         percent       �  ��   PSAP_TRA_FLAG                   
_FillValue        �      units         1      	long_name         Flag for PSAP Transmittance    	frequency                  <  ��   TAS                 
_FillValue        �<    number             	long_name         -True airspeed (dry-air) from turbulence probe      standard_name         platform_speed_wrt_air     	frequency                units         m s-1         �  �    TAS_FLAG                
_FillValue        �      units         1      	long_name         6Flag for True airspeed (dry-air) from turbulence probe     	frequency                   <  ��   CO_AERO                 
_FillValue        �<    number               	long_name         GMole fraction of Carbon Monoxide in air from the AERO AL5002 instrument    standard_name         'mole_fraction_of_carbon_monoxide_in_air    	frequency               units         ppb       �  �,   CO_AERO_FLAG                
_FillValue        �      units         1      	long_name         PFlag for Mole fraction of Carbon Monoxide in air from the AERO AL5002 instrument   	frequency                  <  �   SW_DN_C                 
_FillValue        �<    number          �   	long_name         4Corrected downward short wave irradiance, clear dome   standard_name         !downwelling_shortwave_flux_in_air      	frequency               units         W m-2         �  �X   SW_DN_C_FLAG                
_FillValue        �      units         1      	long_name         =Flag for Corrected downward short wave irradiance, clear dome      	frequency                  <  �H   TDEW_GE                 
_FillValue        �<    number             	long_name         -Dew point from the General Eastern instrument      standard_name         dew_point_temperature      	frequency               units         degK      �  �   TDEW_GE_FLAG                
_FillValue        �      units         1      	long_name         6Flag for Dew point from the General Eastern instrument     	frequency                  <  �t   CAB_TEMP                
_FillValue        �<    number          �   	long_name         &Cabin temperature at the core consoles     standard_name                	frequency               units         degC      �  �   CAB_TEMP_FLAG                   
_FillValue        �      units         1      	long_name         /Flag for Cabin temperature at the core consoles    	frequency                  <  �   BTHEIM_U                
_FillValue        �<    number             	long_name         >Uncorrected brightness temperature from the Heimann radiometer     standard_name                	frequency               units         degK      �  ��   BTHEIM_U_FLAG                   
_FillValue        �      units         1      	long_name         GFlag for Uncorrected brightness temperature from the Heimann radiometer    	frequency                  <  ��   P0_S10                  
_FillValue        �<    number             	long_name         GCalibrated differential pressure between centre(P0) port and S10 static    standard_name                	frequency                units         hPa       �  �   P0_S10_FLAG                 
_FillValue        �      units         1      	long_name         PFlag for Calibrated differential pressure between centre(P0) port and S10 static   	frequency                   <  ��   AOA                 
_FillValue        �<    number          $   	long_name         OAngle of attack from the turbulence probe (positive, flow upwards wrt a/c axes)    standard_name                	frequency                units         degree        �  �4   AOA_FLAG                
_FillValue        �      units         1      	long_name         XFlag for Angle of attack from the turbulence probe (positive, flow upwards wrt a/c axes)   	frequency                   <  �$   AOSS                
_FillValue        �<    number          %   	long_name         FAngle of sideslip from the turbulence probe (positive, flow from left)     standard_name                	frequency                units         degree        �  �`   	AOSS_FLAG                   
_FillValue        �      units         1      	long_name         OFlag for Angle of sideslip from the turbulence probe (positive, flow from left)    	frequency                   <  �P   RED_DN_C                
_FillValue        �<    number          �   	long_name         2Corrected downward short wave irradiance, red dome     standard_name                	frequency               units         W m-2         �  ��   RED_DN_C_FLAG                   
_FillValue        �      units         1      	long_name         ;Flag for Corrected downward short wave irradiance, red dome    	frequency                  <  �|   PSAP_LIN                
_FillValue        �<    number          �   	long_name         >Uncorrected absorption coefficient at 565nm, linear, from PSAP     standard_name                	frequency               units         m-1       �  ��   PSAP_LIN_FLAG                   
_FillValue        �      units         1      	long_name         GFlag for Uncorrected absorption coefficient at 565nm, linear, from PSAP    	frequency                  <  ��   RED_UP_C                
_FillValue        �<    number          �   	long_name         0Corrected upward short wave irradiance, red dome   standard_name                	frequency               units         W m-2         �  ��   RED_UP_C_FLAG                   
_FillValue        �      units         1      	long_name         9Flag for Corrected upward short wave irradiance, red dome      	frequency                  <  ��   HGT_RADR                
_FillValue        �<    number          ?   	long_name         .Radar height from the aircraft radar altimeter     standard_name         height     	frequency               units         m         �  �   HGT_RADR_FLAG                   
_FillValue        �      units         1      	long_name         7Flag for Radar height from the aircraft radar altimeter    	frequency                  <  �    PS_RVSM                 
_FillValue        �<    number          @   	long_name         8Static pressure from the aircraft RVSM (air data) system   standard_name         air_pressure   	frequency                units         hPa       �  �<   PS_RVSM_FLAG                
_FillValue        �      units         1      	long_name         AFlag for Static pressure from the aircraft RVSM (air data) system      	frequency                   <  �,   Q_RVSM                  
_FillValue        �<    number          A   	long_name         MPitot static pressure inverted from RVSM (air data) system indicated airspeed      standard_name                	frequency                units         hPa       �  �h   Q_RVSM_FLAG                 
_FillValue        �      units         1      	long_name         VFlag for Pitot static pressure inverted from RVSM (air data) system indicated airspeed     	frequency                   <  �X   PALT_RVS                
_FillValue        �<    number          B   	long_name         :Pressure altitude from the aircraft RVSM (air data) system     standard_name         barometric_altitude    	frequency                units         m         �  ��   PALT_RVS_FLAG                   
_FillValue        �      units         1      	long_name         CFlag for Pressure altitude from the aircraft RVSM (air data) system    	frequency                   <  ��   CAB_PRES                
_FillValue        �<    number          C   	long_name         Cabin pressure     standard_name                	frequency               units         hPa       �  ��   CAB_PRES_FLAG                   
_FillValue        �      units         1      	long_name         Flag for Cabin pressure    	frequency                  <  ��   V_C                 
_FillValue        �<    number          �   	long_name         6Northward wind component from turbulence probe and GIN     standard_name         northward_wind     	frequency                units         m s-1         �  ��   V_C_FLAG                
_FillValue        �      units         1      	long_name         ?Flag for Northward wind component from turbulence probe and GIN    	frequency                   <  ��   U_C                 
_FillValue        �<    number          �   	long_name         5Eastward wind component from turbulence probe and GIN      standard_name         eastward_wind      	frequency                units         m s-1         �     U_C_FLAG                
_FillValue        �      units         1      	long_name         >Flag for Eastward wind component from turbulence probe and GIN     	frequency                   <    W_C                 
_FillValue        �<    number          �   	long_name         5Vertical wind component from turbulence probe and GIN      standard_name         upward_air_velocity    	frequency                units         m s-1         � D   W_C_FLAG                
_FillValue        �      units         1      	long_name         >Flag for Vertical wind component from turbulence probe and GIN     	frequency                   < 4   V_NOTURB                
_FillValue        �<    number               	long_name         BNorthward wind component derived from aircraft instruments and GIN     standard_name         northward_wind     	frequency               units         m s-1         � p   V_NOTURB_FLAG                   
_FillValue        �      units         1      	long_name         KFlag for Northward wind component derived from aircraft instruments and GIN    	frequency                  < `   U_NOTURB                
_FillValue        �<    number               	long_name         AEastward wind component derived from aircraft instruments and GIN      standard_name         eastward_wind      	frequency               units         m s-1         � �   U_NOTURB_FLAG                   
_FillValue        �      units         1      	long_name         JFlag for Eastward wind component derived from aircraft instruments and GIN     	frequency                  < �   PSP_TURB                
_FillValue        �<    number             	long_name         OPitot-static pressure from centre-port measurements corrrected for AoA and AoSS    standard_name                	frequency                units         hPa       � �   PSP_TURB_FLAG                   
_FillValue        �      units         1      	long_name         XFlag for Pitot-static pressure from centre-port measurements corrrected for AoA and AoSS   	frequency                   < �   SO2_TECO                
_FillValue        �<    number          �   	long_name         ?Mole fraction of Sulphur Dioxide in air from TECO 43 instrument    standard_name         'mole_fraction_of_sulphur_dioxide_in_air    	frequency               units         ppb       � �   SO2_TECO_FLAG                   
_FillValue        �      units         1      	long_name         HFlag for Mole fraction of Sulphur Dioxide in air from TECO 43 instrument   	frequency                  < �   LAT_GIN                 
_FillValue        �<    number          b   	long_name         ;Latitude from POS AV 510 GPS-aided Inertial Navigation unit    standard_name         latitude   	frequency                units         degree_north      �     LAT_GIN_FLAG                
_FillValue        �      units         1      	long_name         DFlag for Latitude from POS AV 510 GPS-aided Inertial Navigation unit   	frequency                   <    LON_GIN                 
_FillValue        �<    number          c   	long_name         <Longitude from POS AV 510 GPS-aided Inertial Navigation unit   standard_name         	longitude      	frequency                units         degree_east       � L   LON_GIN_FLAG                
_FillValue        �      units         1      	long_name         EFlag for Longitude from POS AV 510 GPS-aided Inertial Navigation unit      	frequency                   < 	<   ALT_GIN                 
_FillValue        �<    number          d   	long_name         ;Altitude from POS AV 510 GPS-aided Inertial Navigation unit    standard_name         altitude   	frequency                units         m         � 	x   ALT_GIN_FLAG                
_FillValue        �      units         1      	long_name         DFlag for Altitude from POS AV 510 GPS-aided Inertial Navigation unit   	frequency                   < 
h   VELN_GIN                
_FillValue        �<    number          e   	long_name         JAircraft velocity north from POS AV 510 GPS-aided Inertial Navigation unit     standard_name                	frequency                units         m s-1         � 
�   VELN_GIN_FLAG                   
_FillValue        �      units         1      	long_name         SFlag for Aircraft velocity north from POS AV 510 GPS-aided Inertial Navigation unit    	frequency                   < �   VELE_GIN                
_FillValue        �<    number          f   	long_name         IAircraft velocity east from POS AV 510 GPS-aided Inertial Navigation unit      standard_name                	frequency                units         m s-1         � �   VELE_GIN_FLAG                   
_FillValue        �      units         1      	long_name         RFlag for Aircraft velocity east from POS AV 510 GPS-aided Inertial Navigation unit     	frequency                   < �   VELD_GIN                
_FillValue        �<    number          g   	long_name         IAircraft velocity down from POS AV 510 GPS-aided Inertial Navigation unit      standard_name                	frequency                units         m s-1         � �   VELD_GIN_FLAG                   
_FillValue        �      units         1      	long_name         RFlag for Aircraft velocity down from POS AV 510 GPS-aided Inertial Navigation unit     	frequency                   < �   ROLL_GIN                
_FillValue        �<    number          h   	long_name         SRoll angle from POS AV 510 GPS-aided Inertial Nav. unit (positive for left wing up)    standard_name         platform_roll_angle    	frequency                units         degree        � (   ROLL_GIN_FLAG                   
_FillValue        �      units         1      	long_name         \Flag for Roll angle from POS AV 510 GPS-aided Inertial Nav. unit (positive for left wing up)   	frequency                   <    PTCH_GIN                
_FillValue        �<    number          i   	long_name         OPitch angle from POS AV 510 GPS-aided Inertial Nav. unit (positive for nose up)    standard_name         platform_pitch_angle   	frequency                units         degree        � T   PTCH_GIN_FLAG                   
_FillValue        �      units         1      	long_name         XFlag for Pitch angle from POS AV 510 GPS-aided Inertial Nav. unit (positive for nose up)   	frequency                   < D   HDG_GIN                 
_FillValue        �<    number          j   	long_name         5Heading from POSAV GPS-aided Inertial Navigation unit      standard_name         platform_yaw_angle     	frequency                units         degree        � �   HDG_GIN_FLAG                
_FillValue        �      units         1      	long_name         >Flag for Heading from POSAV GPS-aided Inertial Navigation unit     	frequency                   < p   TRCK_GIN                
_FillValue        �<    number          l   	long_name         GAircraft track angle from POS AV 510 GPS-aided Inertial Navigation unit    standard_name         platform_course    	frequency                units         degree        � �   TRCK_GIN_FLAG                   
_FillValue        �      units         1      	long_name         PFlag for Aircraft track angle from POS AV 510 GPS-aided Inertial Navigation unit   	frequency                   < �   GSPD_GIN                
_FillValue        �<    number          m   	long_name         >Groundspeed from POS AV 510 GPS-aided Inertial Navigation unit     standard_name         platform_speed_wrt_ground      	frequency                units         m s-1         � �   GSPD_GIN_FLAG                   
_FillValue        �      units         1      	long_name         GFlag for Groundspeed from POS AV 510 GPS-aided Inertial Navigation unit    	frequency                   < �   ROLR_GIN                
_FillValue        �<    number          n   	long_name          Rate-of-change of GIN roll angle   standard_name         platform_roll_rate     	frequency                units         
degree s-1        �    ROLR_GIN_FLAG                   
_FillValue        �      units         1      	long_name         )Flag for Rate-of-change of GIN roll angle      	frequency                   < �   PITR_GIN                
_FillValue        �<    number          o   	long_name         !Rate-of-change of GIN pitch angle      standard_name         platform_pitch_rate    	frequency                units         
degree s-1        � 0   PITR_GIN_FLAG                   
_FillValue        �      units         1      	long_name         *Flag for Rate-of-change of GIN pitch angle     	frequency                   <     HDGR_GIN                
_FillValue        �<    number          p   	long_name         Rate-of-change of GIN heading      standard_name         platform_yaw_rate      	frequency                units         
degree s-1        � \   HDGR_GIN_FLAG                   
_FillValue        �      units         1      	long_name         &Flag for Rate-of-change of GIN heading     	frequency                   < L   ACLF_GIN                
_FillValue        �<    number          q   	long_name         JAcceleration along the aircraft longitudinal axis (GIN) (positive forward)     standard_name                	frequency                units         m s-2         � �   ACLF_GIN_FLAG                   
_FillValue        �      units         1      	long_name         SFlag for Acceleration along the aircraft longitudinal axis (GIN) (positive forward)    	frequency                   < x   ACLS_GIN                
_FillValue        �<    number          r   	long_name         JAcceleration along the aircraft transverse axis (GIN) (positive starboard)     standard_name                	frequency                units         m s-2         � �   ACLS_GIN_FLAG                   
_FillValue        �      units         1      	long_name         SFlag for Acceleration along the aircraft transverse axis (GIN) (positive starboard)    	frequency                   < �   ACLD_GIN                
_FillValue        �<    number          s   	long_name         CAcceleration along the aircraft vertical axis (GIN) (positive down)    standard_name                	frequency                units         m s-2         � �   ACLD_GIN_FLAG                   
_FillValue        �      units         1      	long_name         LFlag for Acceleration along the aircraft vertical axis (GIN) (positive down)   	frequency                   < �   SW_UP_C                 
_FillValue        �<    number          �   	long_name         2Corrected upward short wave irradiance, clear dome     standard_name         upwelling_shortwave_flux_in_air    	frequency               units         W m-2         �    SW_UP_C_FLAG                
_FillValue        �      units         1      	long_name         ;Flag for Corrected upward short wave irradiance, clear dome    	frequency                  < �   EXX_JCI                 
_FillValue        �<    number               	long_name         DRaw data from the Fwd Core Console JCI static monitor, static signal   standard_name                	frequency               units         
adc counts        � 8   EXX_JCI_FLAG                
_FillValue        �      units         1      	long_name         MFlag for Raw data from the Fwd Core Console JCI static monitor, static signal      	frequency                  < (   WOW_IND                 
_FillValue        �<    number               	long_name         Weight on wheels indicator     standard_name                	frequency               units         -         � d   WOW_IND_FLAG                
_FillValue        �      units         1      	long_name         #Flag for Weight on wheels indicator    	frequency                  < T   
WVSS2F_VMR                  
_FillValue        �<    number               	long_name         ]Water Vapour Measurement from Flush inlet WVSSII serial no. 4229 linearly interpolated to 1Hz      standard_name                	frequency               units         ppmv      � �   WVSS2F_VMR_FLAG                 
_FillValue        �      units         1      	long_name         fFlag for Water Vapour Measurement from Flush inlet WVSSII serial no. 4229 linearly interpolated to 1Hz     	frequency                  < �   
WVSS2R_VMR                  
_FillValue        �<    number               	long_name         aWater Vapour Measurement from Rosemount inlet WVSSII serial no. 0388 linearly interpolated to 1Hz      standard_name                	frequency               units         ppmv      � �   WVSS2R_VMR_FLAG                 
_FillValue        �      units         1      	long_name         jFlag for Water Vapour Measurement from Rosemount inlet WVSSII serial no. 0388 linearly interpolated to 1Hz     	frequency                  <  �   CPC_CNTS                
_FillValue        �<    number               	long_name         5Condensation Particle Counts measured by the TSI 3786      standard_name                	frequency            
   units         #         �  �   CPC_CNTS_FLAG                   
_FillValue        �      units         1      	long_name         >Flag for Condensation Particle Counts measured by the TSI 3786     	frequency            
      < !�   TDEW_CR2                
_FillValue        �<    number             	long_name         6Mirror Temperature measured by the Buck CR2 Hygrometer     standard_name         dew_point_temperature      	frequency               units         degK      � "   TDEW_CR2_FLAG                   
_FillValue        �      units         1      	long_name         ?Flag for Mirror Temperature measured by the Buck CR2 Hygrometer    	frequency                  < #   TDEW_C_U                
_FillValue        �<    number             	long_name         4Uncertainty estimate for Buck CR2 Mirror Temperature   standard_name                	frequency               units         degK      � #@   TDEW_C_U_FLAG                   
_FillValue        �      units         1      	long_name         =Flag for Uncertainty estimate for Buck CR2 Mirror Temperature      	frequency                  < $0   VMR_CR2                 
_FillValue        �<    number             	long_name         9Water vapour volume mixing ratio measured by the Buck CR2      standard_name         #volume_mixing_ratio_of_water_in_air    	frequency               units         ppmv      � $l   VMR_CR2_FLAG                
_FillValue        �      units         1      	long_name         BFlag for Water vapour volume mixing ratio measured by the Buck CR2     	frequency                  < %\   VMR_C_U                 
_FillValue        �<    number             	long_name         RUncertainty estimate for water vapour volume mixing ratio measured by the Buck CR2     standard_name                	frequency               units         ppmv      � %�   VMR_C_U_FLAG                
_FillValue        �      units         1      	long_name         [Flag for Uncertainty estimate for water vapour volume mixing ratio measured by the Buck CR2    	frequency                  < &�   TDEWCR2C                
_FillValue        �<    number               	long_name         CCorrected dew point temperature measured by the Buck CR2 Hygrometer    standard_name         dew_point_temperature      	frequency               units         degK      � &�   TDEWCR2C_FLAG                   
_FillValue        �      units         1      	long_name         LFlag for Corrected dew point temperature measured by the Buck CR2 Hygrometer   	frequency                  < '�   	O3_2BTECH                   
_FillValue        �<    number               	long_name         8Mole fraction of Ozone in air from the 2BTech instrument   standard_name         mole_fraction_of_ozone_in_air      	frequency               units         ppb       � '�   O3_2BTECH_FLAG                  
_FillValue        �      units         1      	long_name         AFlag for Mole fraction of Ozone in air from the 2BTech instrument      	frequency                  < (�   NV_TWC_U                
_FillValue        �<    number               	long_name         AUncorrected total condensed water content from the Nevzorov probe      standard_name                	frequency            @   units         gram m-3      � )   NV_TWC_U_FLAG                   
_FillValue        �      units         1      	long_name         JFlag for Uncorrected total condensed water content from the Nevzorov probe     	frequency            @      < *   	NV_LWC1_U                   
_FillValue        �<    number               	long_name         HUncorrected liquid water content from the Nevzorov probe (1st collector)   standard_name         )mass_concentration_of_liquid_water_in_air      	frequency            @   units         gram m-3      � *H   NV_LWC1_U_FLAG                  
_FillValue        �      units         1      	long_name         QFlag for Uncorrected liquid water content from the Nevzorov probe (1st collector)      	frequency            @      < +8   	NV_LWC2_U                   
_FillValue        �<    number               	long_name         HUncorrected liquid water content from the Nevzorov probe (2nd collector)   standard_name         )mass_concentration_of_liquid_water_in_air      	frequency            @   units         gram m-3      � +t   NV_LWC2_U_FLAG                  
_FillValue        �      units         1      	long_name         QFlag for Uncorrected liquid water content from the Nevzorov probe (2nd collector)      	frequency            @      < ,d   NV_TWC_C                
_FillValue        �<    number               	long_name         ?Corrected total condensed water content from the Nevzorov probe    standard_name                	frequency            @   units         gram m-3      � ,�   NV_TWC_C_FLAG                   
_FillValue        �      units         1      	long_name         HFlag for Corrected total condensed water content from the Nevzorov probe   	frequency            @      < -�   	NV_LWC1_C                   
_FillValue        �<    number               	long_name         FCorrected liquid water content from the Nevzorov probe (1st collector)     standard_name         )mass_concentration_of_liquid_water_in_air      	frequency            @   units         gram m-3      � -�   NV_LWC1_C_FLAG                  
_FillValue        �      units         1      	long_name         OFlag for Corrected liquid water content from the Nevzorov probe (1st collector)    	frequency            @      < .�   	NV_LWC2_C                   
_FillValue        �<    number               	long_name         FCorrected liquid water content from the Nevzorov probe (2nd collector)     standard_name         )mass_concentration_of_liquid_water_in_air      	frequency            @   units         gram m-3      � .�   NV_LWC2_C_FLAG                  
_FillValue        �      units         1      	long_name         OFlag for Corrected liquid water content from the Nevzorov probe (2nd collector)    	frequency            @      < /�  ?/  ?0  ?1  ?2  ?3  ?4  ?5  ?6  ?7  ?8  ?9  ?:  ?;  ?<  ?=  ?>  ??  ?@  ?A  ?B  ?C  ?D  ?E  ?F  ?G  ?H  ?I  ?J  ?K  ?L  ?M  ?N  ?O  ?P  ?Q  ?R  ?S  ?T  ?U  ?V  ?W  ?X  ?Y  ?Z  ?[  ?\  ?]  ?^  ?_  ?`  ?a  ?b  ?c  ?d  ?e  ?f  ?g  ?h  ?i  ?j�< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< ������������������������������������������������������������D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��D��C��C���C���C��C���C��C��+C��*C���C���C��C��GC��1C�� C��*C��GC��MC��lC��NC��C��cC��C��C��C��C��C��C���C���C���C���C��,C��,C��+C��3C��RC��rC��@C��C��9C��;C��UC���C���C���C���C��C��C���C��C���C��C���C���C���C�� C��C��<C��C��<��~=-e=.i=α=N!<�]=�{<��C=�={
<���= a�= ��=d= f<���=(=�!=Hl<��<��P<�U	<�j=<�x�<�p<�O�<���<�s<�)[= ��=�?<�6�<��=�<�rp=k�= u�<�\�<�~B<���<�,�=�=��=s�=	�=�#<�,q<��{<��Z<�/�<�S~<�)�=/#=��<��<���=��=m<��<�P                                                            �2�x���򫐺�?��I�8���,�Z���F� ��R���P�0�3
��^����c�����UK��2Zh�����a �,h�n�0��_����\�j���]W���a���������7  �'�X�YV�?�P�T��S���P�QV �k^ػ�� ������p�������Ƙ��,��,=軑{�sjлg�X��̬�vػyf8��(���S/��a �_���/b�t�@�r��                                                            CEM�CEM�CENCEM�CEM�CEN�CEN�CEN�CENQCENQCEN�CEN�CEN�CEN�CEN�CEN�CEN�CEOCEN�CEO[CEO
CEO]CEO^CEO@CEOzCEO�CEO`CEO�CEO�CEO�CEPCEPuCEPuCEPtCEP~CEP�CEP�CEP�CEPACEP�CEP�CEP�CEQCEQ�CEQ�CEQ�CEQ[CEQrCEQ(CEQ[CEQ�CEQqCEQ�CEQ�CEQ�CEQ�CERCERaCERCER 3֑L3֑L3֦3֦3֑L3֑L3֦3֦3֑L3֑L3֑L3ֺ�3֦3֑L3ֺ�3֦3֦3֑L3֑L3֑L3֑L3֦3ֺ�3֦3֦3ֺ�3֦3֦3�|�3ֺ�3֦3ֺ�3֦3֑L3֦3֦3֑L3֑L3֦3ֺ�3֦3֑L3֦3֦3֦3ֺ�3ֺ�3֦3ֺ�3ֺ�3֦3ֺ�3ֺ�3֑L3ֺ�3֦3֦3֦3֑L3�|��Sf|���}������}�Sf|�Sf|�Sf|�Sf|�Sf|���������}�Sf|�Sf|�Sf|���}�Sf|�Sf|����Sf|���}���}�Sf|�Sf|�Sf|�Sf|�Sf|�Sf|������}�Sf|�Sf|�Sf|���}�Sf|�Sf|���}������}���}�Sf|9�����Sf|�Sf|���}���}����Sf|�Sf|�Sf|�Sf|����������Sf|����Sf|�Sf|����Sf|�Sf|���}���}�Sf|�
�޸Sf|���}���}�Sf|���}�Sf|���}���}�Sf|�Sf|�Sf|�
�޷���Sf|���}������}�Sf|���}���}���}�Sf|���}���}�Sf|�Sf|�Sf|�
�޸��}������}�
�޸��}�Sf|����Sf|�Sf|�Sf|�Sf|����Sf|������}�Sf|�Sf|�Sf|���}����Sf|���}�Sf|����Sf|����< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< ������������������������������������������������������������CD,4CD,yCD,�CD,4CD,4CD,4CD,�CD-CD,yCD-�CD-�CD,yCD,yCD,�CD,4CD,yCD,5CD,yCD,yCD-CD,yCD-HCD-HCD-CD-HCD,4CD+�CD,4CD+�CD+�CD,4CD,yCD,4CD+�CD,5CD+�CD,yCD,�CD,yCD,4CD+�CD-CD,yCD,yCD,�CD-HCD-HCD,4CD,yCD+�CD,4CD+hCD+�CD,4CD-CD-CD-HCD-HCD,�CD,�Ao^�Ao^8Aoa[Ao\�Ao^8Ao`Ao\VAo\�Ao\VAo\�Ao\�Ao[�Ao\�AoZtAoW�AoZtAoY3AoY�AoY3Ao[AoY3Ao\VAoWQAoWQAoWQAoY3AoVAoV�AoVAoWQAoT�AoT�AoUoAoUoAoT.AoVAoUoAoT.AoT�AoT.AoUoAoT�AoQ�AoS�AoT�AoT.AoT.AoWQAoV�AoUoAoUoAoT.AoVAoWQAoT.AoW�AoW�AoY3AoY3Ao[                                                            Cc��Cc��Cc�bCc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc�0Cc�0Cc��Cc�bCc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��Cc��                                                            >��=>�k�>��>���>��q>��$>���>��$>��$>��$>��$>���>��>�`�>���>�4X>���>��s>�>�`�>�`�>��>��&>�`�>���>�J�>�>�`�>�`�>��&>��q>��&>���>�J�>���>��s>���>�w?>��>�?�>��>�l�>���>��s>�`�>��s>���>�VA>��&>��>�?�>�4X>�`�>��q>���>���>�UV>�UV>�k�>�UV�������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������{] ������{] ��������{] ����{] ��������{] ����{] �{] ������������{] ����{] ����{] �{] �{] ����{] �{] ����{] �{] �����{] �{] ��������������������{] ��������������{] �������{] ����{] ��< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< ������������������������������������������������������������E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�E�D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`D}Y`                                                            E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��E9��������������������������������������������������������������������������������������������������������������������������������������������������������������������������������������                                                            Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�Du!�                                                            �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �m� �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������  �  �  �(  �  �  �  �  �  �  �  �  �   �   �   �  �  �  �  �  �   �   �  �  �  �  �  �   �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �   �  �  �  �  �  �  �  �  �  �  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�                                                              �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �������������������������������������������������������������< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< �< 7��A7�;C7�L7݉t7�F7���7ݹ77�B7ݐd7݇�7�ڧ7�6�7ݗ7�4�7��t7�a47�{�7�?7ݦ7ݤH7ݻ�7�P7ݕ�7�m�7�u�7ݠ7�!�7ݘ�7ݐ�7��7ݛ�7��I7�c7�?�7�|[7ݿ7�Nz7�	7�m7��D7ݾ7�*�7�u�7���7�[�7�m�7��B7�$�7��07���7��E7�ͦ7�17ݻ�7ݑc7��(7ݎ&7ݬ�7��Y7�g:�x�`�z��yj��yG&�yJ��z5n�y�W�y�W�y���y|��yS|�y+W�y+'�y��y���zx`�y��z�y�x�yѝ�z�yA�y�j�z]ٸztJ�zYH�z'+�z�)�ztR�y���y�}�y爸zb��y�?�zu�yë�z"�y���z�ظzOv�y��y�4�y���y���ym��y���z@޸y�ظy�	�y��y��zA!�y��yz��zF�y���yN�y1I�ycE�xk����@��뒸�����3���l������o���-���B���R���,���
����� ���ǀ��w��皸��������r��
��t���{����;	��߸�������5����!������О��0����E��MN��������
�����y�������B��۸���������,���U����������u���i��	��!����^��O������S�������ָ�SV8&��8&�S8&�k8&�8&��8&�8&��8&b8&��8&�28&ȑ8&s8&��8&x�8&�-8&�j8&�J8&�8&�g8&��8&ɴ8'98&�R8&��8&��8&��8&��8&�j8&�T8&�8&�?8&�8&�B8&�	8&��8&��8&��8&�8&�78&ɷ8&��8&qs8&�8&aa8&��8&��8&ɐ8&�S8&Ӯ8&��8&��8&�D8&y�8&��8&�"8&֍8&��8&��8&��8'���J���ۊ���W��q��r븀�i���������9�����wr��c(��c���Ḁ�v��
⸀�9���ɸ��8�������K��n̸�Ĝ���Ӹ�	/���x���E��6C��	3�������I����� 8���<���h�����ߩ���b��P���h���8���[���������������͸�����s����������N<���<�������=��q���ָ�tڸ�f��`�����M�饩��ݸ�{i��G^�����WD�鏹��{��f���j���aM��Y/����5��������e��zS�������0ø���Ř���������h��g��춸�t߸��銆�����	��͸騙��x��W���:������u#��u��Т��2�鋓�钎��۸鴩��pE�鄍��UǸ������`��	���H�阛��I��}V��}��J
include "unit_cell.f90"

program main
use unit_cell_class
implicit none
	type(unit_cell) :: uc1,uc2
	
	! Datos de la estructura del Fe-bcc
	call uc1%set_lpa(2.866)
	call uc1%set_lpb(2.866)
	call uc1%set_lpc(2.866)
	call uc1%set_lta(1.0,0.0,0.0)
	call uc1%set_ltb(0.0,1.0,0.0)
	call uc1%set_ltc(0.0,0.0,1.0)
	call uc1%set_na(2)
	call uc1%add_atom("Fe",1,0.0,0.0,0.0)
	call uc1%add_atom("Fe",1,0.5,0.5,0.5)
	! Generación de la nanopartícula de d=2nm
	call uc1%ellipsoid(2.0,2.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc1%analysis()
	call uc1%print_system("fe_bcc_3nm.xyz")
	! Generación de la nanopartícula de d=3nm
	call uc1%ellipsoid(3.0,3.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc1%analysis()
	call uc1%print_system("fe_bcc_3nm.xyz")
	! Generación de la nanopartícula de d=4nm
	call uc1%ellipsoid(4.0,4.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc1%analysis()
	call uc1%print_system("fe_bcc_4nm.xyz")
	! Generación de la nanopartícula de d=5nm
	call uc1%ellipsoid(5.0,5.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc1%analysis()
	call uc1%print_system("fe_bcc_5nm.xyz")
	! Generación de la nanopartícula de d=6nm
	call uc1%ellipsoid(6.0,6.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc1%analysis()
	call uc1%print_system("fe_bcc_6nm.xyz")
	! Generación de la nanopartícula de d=7nm
	call uc1%ellipsoid(7.0,7.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc1%analysis()
	call uc1%print_system("fe_bcc_7nm.xyz")
	! Generación de la nanopartícula de d=8nm
	call uc1%ellipsoid(8.0,8.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc1%analysis()
	call uc1%print_system("fe_bcc_8nm.xyz")
	! Generación de la nanopartícula de d=9nm
	call uc1%ellipsoid(9.0,9.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc1%analysis()
	call uc1%print_system("fe_bcc_9nm.xyz")
	! Generación de la nanopartícula de d=10nm
	call uc1%ellipsoid(10.0,10.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc1%analysis()
	call uc1%print_system("fe_bcc_10nm.xyz")
	
	! Datos de la estructura del Co-hcp
	call uc2%set_lpa(2.507)
	call uc2%set_lpb(2.507)
	call uc2%set_lpc(2.507)
	call uc2%set_lta(1.0,0.0,0.0)
	call uc2%set_ltb(0.5,0.86602,0.0)
	call uc2%set_ltc(0.0,0.0,1.63288)
	call uc2%set_na(2)
	call uc2%add_atom("Co",1,0.0,0.0,0.0)
	call uc2%add_atom("Co",1,0.33333,0.33333,0.5)
	! Generación de la nanopartícula de d=2nm
	call uc2%ellipsoid(2.0,2.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc2%analysis()
	call uc2%print_system("co_hcp_3nm.xyz")
	! Generación de la nanopartícula de d=3nm
	call uc2%ellipsoid(3.0,3.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc2%analysis()
	call uc2%print_system("co_hcp_3nm.xyz")
	! Generación de la nanopartícula de d=4nm
	call uc2%ellipsoid(4.0,4.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc2%analysis()
	call uc2%print_system("co_hcp_4nm.xyz")
	! Generación de la nanopartícula de d=5nm
	call uc2%ellipsoid(5.0,5.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc2%analysis()
	call uc2%print_system("co_hcp_5nm.xyz")
	! Generación de la nanopartícula de d=6nm
	call uc2%ellipsoid(6.0,6.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc2%analysis()
	call uc2%print_system("co_hcp_6nm.xyz")
	! Generación de la nanopartícula de d=7nm
	call uc2%ellipsoid(7.0,7.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc2%analysis()
	call uc2%print_system("co_hcp_7nm.xyz")
	! Generación de la nanopartícula de d=8nm
	call uc2%ellipsoid(8.0,8.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc2%analysis()
	call uc2%print_system("co_hcp_8nm.xyz")
	! Generación de la nanopartícula de d=9nm
	call uc2%ellipsoid(9.0,9.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc2%analysis()
	call uc2%print_system("co_hcp_9nm.xyz")
	! Generación de la nanopartícula de d=10nm
	call uc2%ellipsoid(10.0,10.0,0.0,0.0,0.0,0.0,0.0,1.0)
	call uc2%analysis()
	call uc2%print_system("co_hcp_10nm.xyz")
	
end program main

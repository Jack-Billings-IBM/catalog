import com.ibm.dbb.build.*

// Copy a entire directory to PDS
  new CopyToPDS().file(new File("dbb/catalog/copybook")).dataset("DFH550.CICS.SDFHSAMP").execute()

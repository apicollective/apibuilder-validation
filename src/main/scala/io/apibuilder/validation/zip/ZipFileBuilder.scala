package io.apibuilder.validation.zip

import java.io.{File, FileInputStream, FileOutputStream}
import java.util.zip.ZipEntry
import java.util.zip.ZipOutputStream

case class ZipFileEntry(name: String, file: File)

case class ZipFileBuilder(
  entries: Seq[ZipFileEntry] = Nil
) {

  def withFile(file: File): ZipFileBuilder = {
    withFile(
      name = file.getName,
      file = file
    )
  }

  def withFile(name: String, file: File): ZipFileBuilder = {
    this.copy(
      entries = entries ++ Seq(ZipFileEntry(name = name, file = file))
    )
  }

  /**
    * Creates a new file with the specified text contents, adding
    * to the zip file.
    */
  def withTextFile(name: String, contents: String): ZipFileBuilder = {
    val file = FileUtil.writeToTempFile(contents = contents)
    this.copy(
      entries = entries ++ Seq(ZipFileEntry(name = name, file = file))
    )
  }

  /**
    * Creates a zip file using a temporary file
    */
  def build(): File = {
    val tmp = File.createTempFile("zipfilebuilder", ".zip")
    tmp.deleteOnExit()
    build(tmp)
    tmp
  }

  /**
    * Writes a zip file to the specified file
    */
  def build(file: File): Unit = {
    val fos = new FileOutputStream(file)
    val zipOS = new ZipOutputStream(fos)
    entries.foreach { file =>
      writeToZipFile(file, zipOS)
    }
    zipOS.close()
    fos.close()
  }

  private[this] def writeToZipFile(entry: ZipFileEntry, zip: ZipOutputStream): Unit = {
    val fis = new FileInputStream(entry.file)
    val zipEntry = new ZipEntry(entry.name)
    zip.putNextEntry(zipEntry)
    val bytes = new Array[Byte](1024)
    var length = fis.read(bytes)
    while (length >= 0) {
      zip.write(bytes, 0, length)
      length = fis.read(bytes)
    }
    zip.closeEntry()
    fis.close()
  }
}
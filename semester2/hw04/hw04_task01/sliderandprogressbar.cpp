#include "sliderandprogressbar.h"
#include "ui_sliderandprogressbar.h"

SliderAndProgressBar::SliderAndProgressBar(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::SliderAndProgressBar)
{
    ui->setupUi(this);
    QObject::connect(ui->horizontalSlider, SIGNAL(valueChanged(int)),
                     ui->progressBar, SLOT(setValue(int)));
}

SliderAndProgressBar::~SliderAndProgressBar()
{
    delete ui;
}

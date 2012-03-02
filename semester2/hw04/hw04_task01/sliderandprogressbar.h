#pragma once

#include <QWidget>

namespace Ui {
    class SliderAndProgressBar;
}

class SliderAndProgressBar : public QWidget
{
    Q_OBJECT

public:
    explicit SliderAndProgressBar(QWidget *parent = 0);
    ~SliderAndProgressBar();

private:
    Ui::SliderAndProgressBar *ui;
};

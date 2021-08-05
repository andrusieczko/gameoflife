package com.github.andrusieczko.gameoflife.views.helloworld;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.html.H1;
import com.vaadin.flow.component.html.Header;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.router.PageTitle;
import com.github.andrusieczko.gameoflife.views.MainLayout;
import com.vaadin.flow.router.RouteAlias;

import java.util.concurrent.atomic.AtomicInteger;

@PageTitle("Hello World")
@Route(value = "hello", layout = MainLayout.class)
@RouteAlias(value = "", layout = MainLayout.class)
public class HelloWorldView extends HorizontalLayout {

    public HelloWorldView() {
        H1 h1 = new H1("Java: Vaadin");
        add(h1);

        AtomicInteger i = new AtomicInteger(0);

        Runnable runnable = () -> {
            while (true) {
                getUI().ifPresent(ui -> ui.access(() -> {
                    h1.setText("Java: Vaadin (" + i.incrementAndGet() + ")");
                }));
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        };

        Thread thread = new Thread(runnable);
        thread.start();
    }

}
